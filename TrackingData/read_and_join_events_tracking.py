
from re import T
import pandas as pd
import os
import numpy as np

TEAM_NAMES = {'Canada': 'Olympic (Women) - Canada', 
              'USA': 'Olympic (Women) - United States',
              'Finland': 'Olympic (Women) - Finland',
              'ROC': 'Olympic (Women) - Olympic Athletes from Russia',
              'Switzerland': 'Olympic (Women) - Switzerland'}

MAX_FRAME_JUMP = 5

# power_play_info_file = 'pp_info.csv'
# power_play_info = pd.read_csv(power_play_info_file)
# power_play_info.head()

# games = power_play_info['game_name.unique']()

# play_by_play_data = pd.read_csv('../pxp_womens_oly_2022_v2.csv')
# play_by_play_data.head()

# game_events = play_by_play_data.loc[(play_by_play_data['game_date'] == '8/2/2022') & 
#                                     ((play_by_play_data['team_name'] == 'Olympic (Women) - Canada') |
#                                         (play_by_play_data['team_name'] == 'Olympic (Women) - United States'))]
# game_events.head()

# period_events = game_events.loc[game_events['period'] == 1]
# period_events.head()

# pp_tracking = pd.read_csv('2022-02-08 Canada at USA/2022-02-08 Canada at USA P1 PP1.csv')
# pp_tracking.head()
# pp_tracking['frame_id'].unique()


# video_shot_information = pd.read_csv('2022-02-08 Canada at USA/videoShotsInfo_2022-02-08 Canada at USA P1 PP1.csv')
# video_shot_information


# matching_event_data = pp_events = period_events.loc[(period_events['clock_seconds'] <= 376) &
#                               (period_events['clock_seconds'] >= 350)]
# matching_event_data.head()

def get_names_date(game):
    spl = game.split(' ')
    date_comps = spl[0].split('-')
    date = '{}/{}/{}'.format(*[dt.strip('0') for dt in date_comps[::-1]])
    team_1 = TEAM_NAMES[spl[1]]
    team_2 = TEAM_NAMES[spl[3]]
    
    return team_1, team_2, date
    
def get_speed(track):
    assert (track['track_id'] == track['track_id'].iloc[0]).all()
    try:
        frame_diff = np.diff(track['frame_id'])
        nonvalid_speed = np.concatenate([[True],frame_diff>MAX_FRAME_JUMP])
        next_valid_speed = np.concatenate([frame_diff<=MAX_FRAME_JUMP,[False]])
        repl = nonvalid_speed & next_valid_speed
        idx = np.where(repl)[0]+1
        
        
        x_spd = np.concatenate([[np.nan],np.diff(track['x_ft'])/frame_diff])*30
        y_spd = np.concatenate([[np.nan],np.diff(track['y_ft'])/frame_diff])*30

        x_spd[repl] = x_spd[idx]
        y_spd[repl] = y_spd[idx]

    except Exception as e:
        print(np.diff(track['frame_id']))
        raise

    return(x_spd, y_spd)


def get_pp_tracking(power_play_info_file = 'pp_info.csv',
    play_by_play_data_file = 'pxp_womens_oly_2022_v2.csv', 
    tracking_dir = './TrackingData/',
    pbp_dir = './'):

    play_by_play_data = pd.read_csv(os.path.join(pbp_dir,play_by_play_data_file))
    power_play_info = pd.read_csv(os.path.join(tracking_dir,power_play_info_file))

    power_play_info['game_name'].loc[power_play_info['game_name']=='2022-02-14 USA at Finland'] = '2022-02-14 Finland at USA'
    power_play_info['relevant_events'] = np.nan
    # print(power_play_info.head())
    relevant_eventses = []
    for ix, pp in power_play_info.iterrows():
        # print(pp)
        relevant_events = None
        game = pp['game_name']
        game_files = os.listdir(os.path.join(tracking_dir,game))
        # print(game_files)
        tracking_data_name = os.path.join(tracking_dir,game,f'{game} P{pp["start_period"]} PP{pp["penalty_number"]}.csv')
        tracking_info_name = os.path.join(tracking_dir,game,f'videoShotsInfo_{game} P{pp["start_period"]} PP{pp["penalty_number"]}.csv')
        roster_info_name = os.path.join(tracking_dir, game, f'{game} roster.csv')
        roster_info = pd.read_csv(roster_info_name, index_col = 0)
        if os.path.split(tracking_data_name)[1] in game_files:
            # print(tracking_data_name)
            tracking_data = pd.read_csv(tracking_data_name)
            tracking_data['x_ft'] = tracking_data['x_ft'] - 7
            tracking_data['x_ft'] = tracking_data['y_ft'] + 4
                        
            tracking_data['vel_x'] = np.nan
            tracking_data['vel_y'] = np.nan
            tr_ids = tracking_data.track_id.unique()
            for tr_id in tr_ids:
                idxs = tracking_data.track_id==tr_id
                track = tracking_data.loc[idxs]
                if len(track) <= 5:
                    continue
                x_spd,y_spd = get_speed(track)
                tracking_data.loc[idxs,'vel_x'] = x_spd
                tracking_data.loc[idxs,'vel_y'] = y_spd
                # break
            # print(tracks.head())

            # print(tracking_info_name) 
            tracking_info = pd.read_csv(tracking_info_name)
            
            # print(tracking_info_name)
            # assert len(tracking_info)==1, 'In this file: {} \nWe have to deal with multiple shots'.format(tracking_info_name)
            # tracking_info = tracking_info.iloc[0,:]
        
            # print(tracking_info)
        else:
            print('Tracking file missing:\n{}'.format(tracking_data_name))
            relevant_eventses.append(None)
            continue
        team_1, team_2, date = get_names_date(game)
        # print('{}\n{}\n{}\n'.format(team_1,team_2,date))
        relevant_events = play_by_play_data.loc[(play_by_play_data['game_date'] == date) & 
                                    ((play_by_play_data['team_name'] == team_1) |
                                        (play_by_play_data['team_name'] == team_2))]
                                        
        relevant_events = relevant_events.loc[((relevant_events['period']>= pp['start_period']) 
                                                & (relevant_events['clock_seconds'] <= pp['start_game_clock_seconds'])) &
                                              ((relevant_events['period']<= pp['end_period']) & 
                                              (relevant_events['clock_seconds'] >= pp['end_game_clock_seconds']))  ]
        
        
        relevant_events.index = np.arange(0, len(relevant_events.index), dtype = 'int')
        
        pl1_jn = roster_info.loc[relevant_events['player_name'],'jn']
        pl1_jn.index = relevant_events.index

        pl2_exists = list(relevant_events['player_name_2'].notna())
        pl2_jn = roster_info.loc[relevant_events['player_name_2'][pl2_exists],'jn']
        
        pl2_jn.index = relevant_events.index[pl2_exists]
        relevant_events['Player_1_num'] = pl1_jn
        relevant_events['Player_2_num'] = pl2_jn
        # pp_length = int(np.ceil(tracking_info['time_end_shot(sec)']))
        tracks = []
        ### Continue from here - need to check if there is a next event
        for rw, event in relevant_events.iterrows():
            rel_shot = 0
            time_bonus = 0 if event['period'] == pp['start_period'] else 1200
            event_frame_time = (pp['start_game_clock_seconds'] - event['clock_seconds'] + time_bonus)*30.0 - 15
            # print(len(relevant_events), rw)
            if rw < (len(relevant_events)-1):
                time_bonus = 0 if relevant_events.loc[rw+1,'period'] == pp['start_period'] else 1200
                next_event_frame_time = (pp['start_game_clock_seconds'] - relevant_events.loc[rw+1,'clock_seconds'] + time_bonus)*30.0 + 15
            else:
                next_event_frame_time = 10000000
            # print(event_frame_time, next_event_frame_time)
            # assert False
            while rel_shot<len(tracking_info.index):
                # print(tracking_info)
                if event_frame_time >= tracking_info.loc[rel_shot,'frame_start_shot']:
                    if event_frame_time >= tracking_info.loc[rel_shot,'frame_end_shot']:
                        rel_shot+=1
                        if rel_shot==len(tracking_info.index):
                            tracks.append(None)
                    else:
                        tracks.append(tracking_data.loc[(tracking_data['frame_id']>=(event_frame_time)) &
                                                     (tracking_data['frame_id']<(next_event_frame_time))])
                        break
                    
                elif next_event_frame_time > tracking_info.loc[rel_shot,'frame_start_shot']:
                    tracks.append(tracking_data.loc[tracking_data['frame_id']<(next_event_frame_time)])
                    break
                else:
                    tracks.append(None)
                    break
        relevant_events['tracks'] = tracks
        
        # print(len(tracks))
        # print(tracks[0])
        # break
        # if relevant_events is None:
            # print('Could find anything for this pp:\n {}'.format(tracking_data_name))
        relevant_eventses.append(relevant_events)
    power_play_info['relevant_events'] = relevant_eventses
    return power_play_info
        # pp_events = period_events.loc[(period_events['clock_seconds'] <= 386) &
                            #   (period_events['clock_seconds'] >= 350)]
        # pp_events.head()

if __name__ == '__main__':
    print(get_pp_tracking().head())





