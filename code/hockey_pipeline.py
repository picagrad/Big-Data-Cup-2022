import numpy as np
import pandas as pd
from scipy.stats import norm
from numba import jit

MAX_TIME = 3
EPS = 1e-7
GG = 32.174

METRICS = ['prob','rink_ctrl','best_case','expected']
TIME_PENALTY = 0.1
MAX_VEL=35.5   # maximum skater velocity in ft/sec
A = 1.3 # acceleration coefficient (not directly acceleration, but more like a speed decay)
TR = 0.189 # reaction time (based on the article Phil sent) 
MM = 0.1 # Coefficient of friction between puck and ice, I'll find the source for this
BETA_PUCK = 0.1322 # Puck air drag coefficient (actuall it's the coefficient divided by the mass so beta = k/m if k is the drag coefficient)
BETA_CTRL = 2.5 # pitch control coefficient used as beta in ice_ctrl_xyt and teamwise_ice_ctrl_xyt, taken from the Spearman paper
X_DECAY = 2000 #value used as decay_x
Y_DECAY = 500 #value used as decay_y
GOALIE_DIST = 8 # maximum reasonable distance for goalie to go away from goal
GLX = 11 # Goalie X coord
GLY = 42.5  # Goalie Y coord
STICK = 5 # Stick length 

class tracks():
    def __init__(self,
                x: np.ndarray, # x locations of players (array or list of floats) 
                y: np.ndarray, # y locations of players (array or list of floats)
                vx: np.ndarray, # x velocity of players (array or list of floats)
                vy: np.ndarray, # y velocity of players (array or list of floats)
                goalie: int, # column number for goalie
                puck: int, # column number for which player has the puck
                off: np.ndarray, # array or list of integers +1 for offence, -1 for defence (or true and false)
                vp: float = 55,
                phi_res: float = 0.05,
                t_res: float = 0.05,
                metric: str = 'expected'):
        assert len(set([len(x),len(y),len(vx),len(vy),len(off)]))<=2
        if not metric in METRICS:
            raise ValueError('Metric choice is not in recognized metric list, please choose another metric')
        self.metric = metric
        self.xp = x[puck]
        self.yp = y[puck]
        self.puck = puck
        self.phi_res = phi_res
        self.off = np.where(off==1,1,-1)
        self.t_res = t_res
        self.vp = vp
        self.x = x
        self.y = y
        self.vx = vx
        self.vy = vy
        self.goalie = goalie
        # self.tracks = pd.DataFrame({'x':x,'y':y,'vx':vx,'vy':vy,'goalie':goalie,'off':off})
        self.grid = np.concatenate([self.one_pass(self, phi)() for phi in np.arange(-np.pi,np.pi+EPS, phi_res)], axis = 0) 
    
    def player_motion(self, alpha: float = ALPHA, t_r: float = TR, vmax: float = MAX_VEL):
        t = np.arange(self.t_res,MAX_TIME, self.t_res)
        self.c_x = np.where(t<t_r, self.x + self.vx * t, self.x + t_r * self.vx + self.vx * (1-np.exp(-alpha * (t-t_r))/alpha))
        self.c_y = np.where(t<t_r, self.y + self.vy * t, self.y + t_r * self.vy + self.vy * (1-np.exp(-alpha * (t-t_r))/alpha))
        self.r = np.where(t<t_r,0,vmax * (t -t_r - (1-np.exp(-alpha * (t-t_r)))/alpha)) 

    
    @staticmethod
    def inside_boards(data: pd.DataFrame,
                target_radius: float = 27.5):
        radius = (data.x<28) * ((data.y>57)*((data.x-28)**2 + (data.y-57)**2)**0.5 + (data.y<28)*((data.x-28)**2 + (28-data.y)**2)**0.5)
        return data.loc[(radius<=target_radius) & (data.x<100) & (data.y<85) & (data.y>0) &(data.x>0)]

    class one_pass():
        def __init__(self, outer_self: 'tracks', phi: float):
            self.t_res = outer_self.t_res
            self.phi = phi
            self.x0 = outer_self.xp
            self.y0 = outer_self.yp
            self.vp = outer_self.vp
            self.grid = self.make_grid()
            self.outside_creese = (self.grid.x-GLX)**2 + (self.grid.y-GLY)**2 > GOALIE_DIST**2
            self.get_metric(outer_self, outer_self.metric)
        
        def make_grid(self):
            t = np.arange(self.t_res,MAX_TIME,self.t_res)
            x, y = self.puck_motion_model(t,self.x0,self.y0)
            return tracks.inside_boards(pd.DataFrame({'x':x,'y':y,'t':t})) 

        def puck_motion_model(self,t: np.ndarray,
                                    phi: float,
                                    mu:float = MM, 
                                    beta: float = BETA_PUCK, 
                                    g: float = GG):
                vx = self.vp*np.sin(self.phi)
                vy = self.vp*np.cos(self.phi)
                
                x =  self.x0 + (vx + mu*g * vx/self.vp/beta) * (1 - np.exp(-beta * t))/beta - (mu*g * vx/self.vp)/beta * t
                y = self.y0 + (vy + mu*g * vy/self.vp/beta) * (1 - np.exp(-beta * t))/beta - (mu*g * vy/self.vp)/beta * t
                
                return x, y
        
        def score_prob(self, decay_x = X_DECAY, decay_y = Y_DECAY):
            # Scoring Probability function 
            x = self.grid.x
            y = self.grid.y
            self.score = (np.abs((x-11)/((42.5-y)**2+(11-x)**2)**0.5)+1)/np.where(x>189,8,4)*np.exp(-((11-x)**2/decay_x +(42.5-y)**2/decay_y))

        def dist_to_xyt(self,x0: float,
                             y0: float,
                             vx: float,
                             vy: float, 
                             vmax: float = MAX_VEL, 
                             alpha:float = A, 
                             t_r: float =  TR):
                # If time is smaller than reaction time, skater keeps going at initial speed
            t = self.grid.t
            tx = self.grid.x
            ty = self.grid.y

            # c_x = (t<t_r) * (x0 + vx * t) + (t>=t_r) * (x0 + t_r * vx + vx * (1-np.exp(-alpha * (t-t_r))/alpha))
            # c_y = (t<t_r) * (y0 + vy * t)  + (t>=t_r) * (y0 + t_r * vy + vy * (1-np.exp(-alpha * (t-t_r))/alpha))
            # r =   (t>=t_r) * vmax * (t -t_r - (1-np.exp(-alpha * (t-t_r)))/alpha)
            c_x = np.where(t<t_r, x0 + vx * t, x0 + t_r * vx + vx * (1-np.exp(-alpha * (t-t_r))/alpha))
            c_y = np.where(t<t_r, y0 + vy * t, y0 + t_r * vy + vy * (1-np.exp(-alpha * (t-t_r))/alpha))
            r = np.where(t<t_r,0,vmax * (t -t_r - (1-np.exp(-alpha * (t-t_r)))/alpha)) 

            remaining_dist = ((tx-c_x)**2 + (ty-c_y)**2)**0.5-r
            return(np.maximum(remaining_dist,EPS))
        
        def get_metric(self,outer_self: 'tracks', metric: str = 'prob'):
            dists = np.array([self.dist_to_xyt(x0,y0,vx,vy) for x0,y0,vx,vy in zip(outer_self.x, outer_self.y, outer_self.vx, outer_self.vy)]).T
            dists[self.outside_creese,outer_self.goalie]  = np.maximum(dists[self.outside_creese,outer_self.goalie], ((self.grid.x[self.outside_creese]-GLX)**2 + (self.grid.y[self.outside_creese]-GLY)**2)**0.5 - GOALIE_DIST)
            ctrl =(dists/MAX_VEL)**(-BETA_CTRL)* outer_self.off.reshape(1,-1)
            all_ctrl = ctrl.sum(1)/np.abs(ctrl).sum(1)
            if metric == 'rink_ctrl':
                self.metric = all_ctrl
                return 0
            dists = np.delete(dists, outer_self.puck,axis = 1)
            base_probs = self.t_res * (norm.cdf(dists/STICK+1)-norm.cdf(dists/STICK-1))/TIME_PENALTY
            ranks = (-base_probs).argsort()
            ranked_probs = np.take_along_axis(base_probs,ranks,0)
            off_mat = np.delete(outer_self.off, outer_self.goalie)[ranks]
            # print(off_mat)
            # print(np.concatenate((np.ones((1,dists.shape[1])),1-ranked_probs[:-1,:]),0).cumprod(1).shape)
            adj_probs = np.concatenate((np.ones((1,dists.shape[1])),1-ranked_probs[:-1,:]),0).cumprod(1)*ranked_probs
            # print(adj_probs.shape)
            adj_pass_off = (adj_probs*(off_mat==1)).sum(1)
            # pass_def = adj_probs[~off_mat] # Not sure we actually need this line
            missed = 1 - adj_probs.sum(1)
            missed = np.append(1,missed[:-1]).cumprod()
            pass_off = adj_pass_off * missed
            if metric == 'prob':
                self.metric = pass_off.sum()  * np.ones(self.grid.x.shape)
            if metric == 'best_case':
                self.score_prob()
                adj_pass_value = self.score*all_ctrl*adj_pass_off
                self.metric = adj_pass_value.max() * np.ones(self.grid.x.shape)

            elif metric == 'expected':
                self.score_prob()
                loc_pass_value = self.score*all_ctrl*pass_off
                self.metric = loc_pass_value.sum() * np.ones(self.grid.x.shape)

        def __call__(self):
            return(np.stack((self.grid.x,self.grid.y,self.grid.t,self.metric),1))    


if __name__ == '__main__':
    x = 200 -np.array([171.4262, 155.6585, 153.7146, 150.5869, 156.3463, 179.8383, 180.8131, 186.6146, 179.9982])
    y=np.array([49.31514, 48.25991, 70.17542, 13.65429, 28.51970, 38.44596, 36.80571, 38.32781, 22.03946])
    vx=np.array([6.725073,  4.964445, -3.097599, 14.252625,  4.286796,  1.925091, -2.295729, -0.294258,  6.464229])
    vy=np.array([-7.1037417,  -7.9677960,  -6.4446342,   6.5618985, -10.9455216,  -4.7444208,  -4.1465373,  -0.3377985, -5.4265284])
    goalie = 7
    puck=np.array([False,False,False,True,False,False,False,False,False])
    off=np.array([-1, -1, 1, 1, -1, 1, -1, -1, 1])
    all_tracks = tracks(x,y,vx,vy,goalie,puck,off,metric='expected')
