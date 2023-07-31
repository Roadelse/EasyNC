# coding=utf-8

import sys
import pickle
sys.path.append('../../cbuild.intel/bin/render-struct-io/')
import render_struct_io
sys.path.append('/sharedata01/zjx/recRoot/GitRepos/froed')
import froed



inDat = 'demo1.run.pk'
fctts = pickle.load(open(inDat, 'rb'))

for fcn, fc in fctts.items():
    if fc.context_type == 'Type':
        if not fc.isrun:
            fc.run()
        render_struct_io.handle_1struct(fc, 4, outDir = '.')
    
