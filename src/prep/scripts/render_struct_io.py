#!/usr/bin/env python3
# coding=utf-8

import os, sys, os.path
import pickle
import glob
import jinja2 as jj2

import jj2_context

def handle_1struct(fc, outDir = '.'):
    ctt = jj2_context.ctt
    ctt['struct_name'] = fc.context_name
    lsp = '!#jj2' # os.environ['jj2_lsp']  # line statement 
    inFile_body = os.path.dirname(__file__) + r'/easync.struct-io-template.inc.jj2'
    inFile_itf = os.path.dirname(__file__) + r'/easync.struct-io-interface.inc.jj2'

    def calls_from_fctt(idx_str, io = 'O'):
        S = """"""
        for vn, v in fc.variables.items():
            vt = v.variable_type
            if v.allocatable:
                apFlag = 'A'
            elif v.pointer:
                apFlag = 'P'
            else:
                apFlag = ''

            if idx_str:
                idx_val_str = f"""toString('(', {idx_str[1:-1]}, ')')"""
            else:
                idx_val_str = "''"

            if vt.startswith('type('):
                vt_struct_name = vt[5:-1]
                S += f"""        call easy{io}{apFlag}_{vt_struct_name}(fname, trim(vname)//{idx_val_str}//'%{vt_struct_name}', data{idx_str}%{vt_struct_name})\n"""
            elif vt.startswith('integer') or vt.startswith('real') or vt.startswith('double') or \
               vt.startswith('character') or vt.startswith('logical') or vt.startswith('complex'):
                S += f"""        call easy{io}{apFlag}(fname, trim(vname)//{idx_val_str}//'%{v.name}', data{idx_str}%{v.name})\n"""
            else:
                print(f"Error! unknwon variable type : {vt}")
                sys.exit(1)
        return S

    ctt['calls_from_fctt'] = calls_from_fctt
    ctt['moduleOrNot'] = 'module' if fc.parent_context.context_type == 'module' else ''

    content_body = open(inFile_body).read()
    content_itf = open(inFile_itf).read()

    template_body = jj2.Template(content_body, line_statement_prefix=lsp)
    template_itf = jj2.Template(content_itf, line_statement_prefix=lsp)

    text_body = template_body.render(ctt)
    text_itf = template_itf.render(ctt)
    ext = os.path.splitext(fc.file)[1]
    with open(outDir + f'/inc.struct-io.{fc.context_name}{ext}', 'w') as f:
        f.write(text_body)
    
    with open(outDir + f'/inc.struct-io-interface.{fc.context_name}{ext}', 'w') as f:
        f.write(text_itf)
    
    return f'inc.struct-io.{fc.context_name}{ext}', f'inc.struct-io-interface.{fc.context_name}{ext}'


def test():
    # be like:
    # python render_struct_io.py ..../cctm5402.run.pk .../froed.proj
    assert len(sys.argv) >= 2  # all arguments are target files
    inDat = sys.argv[1]
    assert os.path.exists(inDat), 'The command line argument must be the target fctts data file! Not existed now!'

    if len(sys.argv) >= 3:
        sys.path.append(sys.argv[2])
    import FortranLogic

    fctts = pickle.load(open(inDat, 'rb'))

    for fcn, fc in fctts.items():
        if isinstance(fc, FortranLogic.FType):
            if not fc.isrun:
                fc.run()
            handle_1struct(fc)


if __name__ == '__main__':
    test()
    
