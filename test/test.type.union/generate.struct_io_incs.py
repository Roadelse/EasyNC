# coding=utf-8

import sys, os.path
import pickle

render_struct_io_dirs = ['../../cbuild.intel/bin/render-struct-io', '../../cbuild.gnu/bin/render-struct-io']
for d in render_struct_io_dirs:
    if os.path.exists(d):
        sys.path.append(d)
        break
import render_struct_io

froed_dirs = [r'D:\recRoot\GitRepos\froed', # zjD windows
              '/mnt/d/recRoot/GitRepos/froed', # zjD wsl
              '/sharedata01/zjx/recRoot/GitRepos/froed'  # qlab
              ]
for fd in froed_dirs:
    if os.path.exists(fd):
        sys.path.append(fd)
        break
import froed



inDat = 'demo1.run.pk'
fctts = pickle.load(open(inDat, 'rb'))

for fcn, fc in fctts.items():
    if fc.context_type == 'Type':
        if not fc.isrun:
            fc.run()
        render_struct_io.handle_1struct(fc, 4, outDir = '.')
    
