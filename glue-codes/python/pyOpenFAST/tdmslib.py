# -*- coding: utf-8 -*-
"""
Created on Wed Jun  5 11:26:52 2024

@author: schamot

Description:
    Code takes a .tdms file path as input and outputs the data as a dictionary.
    
    Parameters
    ----------
    path : str
        path to the .tdms file.

    Returns
    -------
    pyDict : dict
        Python dictionary of the tdms file.

"""

import nptdms

def parse_tdms(filename: str):
    output = TdmsToDict(filename)["Winch System"]
    x = output["x"]
    y = output["y"]
    z = output["z"]
    phi = output["phi"]
    theta = output["theta"]
    psi = output["psi"]
    return {
        "x": x,
        "y": y,
        "z": z,
        "phi": phi,
        "theta": theta,
        "psi": psi,
    }

def TdmsToDict(path):
    '''
    Code takes a .tdms file path as input and outputs the data as a dictionary.

    Parameters
    ----------
    path : str
        path to the .tdms file.

    Returns
    -------
    pyDict : dict
        Python dictionary of the tdms file.

    '''
    # open/read the tdms file
    tdms_file = nptdms.TdmsFile.read(path)

    # Set up python dictionary
    pyDict = {}
    # Get group names
    for group in tdms_file.groups():
        #print(f'''Group: {group.name}''')
        # Create group dictionary key
        pyDict[group.name] = {}
        # Get channel names per group
        for channel in group.channels():
            #print(f'''\tChannel: {channel.name}''')
            pyDict[group.name][channel.name] = channel[:]
            # if the data has properties add them
            if channel.properties != {}:
                pyDict[group.name][channel.name + "_properties"] = channel.properties

    return pyDict

if __name__ == "__main__":
    parse_tdms("Full_10N_Wrench_1.tdms")
