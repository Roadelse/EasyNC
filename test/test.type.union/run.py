#!/usr/bin/env python3
# coding=utf-8

import sys, platform
import os.path
import glob

import rdee

sys.path.append('/mnt/d/recRoot/GitRepos/froed')
import froed

configD = {
    'outDir' : '.',
    'cpp_defs' : {},
    'Idirs' : [],
#    'one_by_one' : True,
    'debug' : False,
    'mediumDir' : '.',
    'split_run' : False, # True,
    'just_run' : False # True
}

ffiles = ['m1.ori.F']

froed.analyze('test', ffiles, configD)
