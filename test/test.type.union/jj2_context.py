# coding=utf-8


def get_rank_from_nd(n: int):
    if n == 0:
        return ''
    
    colons = [':' for _ in range(n)]
    rst_str = '(' + ','.join(colons) + ')'

    return rst_str


def render_arrayShape_from_enc_iaaT_2d(n: int):
    ulb_by_dims = [f'enc_iaaT_2d(1,{i}):enc_iaaT_2d(2,{i})' for i in range(1, n+1)]
    rst_str = ','.join(ulb_by_dims)

    return rst_str

def render_arrayShape_from_shape(s: str, n: int):
    size_by_dims = [f'{s}({i})' for i in range(1, n+1)]
    rst_str = ','.join(size_by_dims)

    return rst_str

def get_b_shape(n: int):
    # 'size(val(1,1,:,1))'
    if n == 1:
        return '(size(val))'
    elif n == 2:
        return '(size(val(:,1)), size(val(1,:)))'
    elif n == 3:
        return '(size(val(:,1,1)), size(val(1,:,1)), size(val(1,1,:)))'
    elif n == 4:
        return '(size(val(:,1,1,1)), size(val(1,:,1,1)), size(val(1,1,:,1)), size(val(1,1,1,:)))'
    elif n == 5:
        return '(size(val(:,1,1,1,1)), size(val(1,:,1,1,1)), size(val(1,1,:,1,1)), size(val(1,1,1,:,1)), size(val(1,1,1,1,:)))'
    elif n == 6:
        return '(size(val(:,1,1,1,1,1)), size(val(1,:,1,1,1,1)), size(val(1,1,:,1,1,1)), size(val(1,1,1,:,1,1)), size(val(1,1,1,1,:,1)), size(val(1,1,1,1,1,:)))'
    elif n == 7:
        return '(size(val(:,1,1,1,1,1,1)), size(val(1,:,1,1,1,1,1)), size(val(1,1,:,1,1,1,1)), size(val(1,1,1,:,1,1,1)), size(val(1,1,1,1,:,1,1)), size(val(1,1,1,1,1,:,1)), size(val(1,1,1,1,1,1,:)))'

    


ctt = {'get_rank_from_nd' : get_rank_from_nd, 
       'render_arrayShape_from_shape' : render_arrayShape_from_shape,
       'render_arrayShape_from_enc_iaaT_2d' : render_arrayShape_from_enc_iaaT_2d,
       'get_b_shape' : get_b_shape,
       'ftypes' : {
            'int4' : 'integer(kind=4)', 
            'int8' : 'integer(kind=8)',
            'real4' : 'real(kind=4)',
            'real8' : 'real(kind=8)',
            'logical' : 'logical',
            'string' : 'character(*)',
            'complex4' : 'complex(kind=4)',
            'complex8' : 'complex(kind=8)'
            }, 
       'nf90_types' : {
            'int4' : 'NF90_INT', 
            'int8' : 'NF90_INT64',
            'real4' : 'NF90_FLOAT',
            'real8' : 'NF90_DOUBLE',
            'string' : 'NF90_CHAR'  # need to be handled with ascending dimensions
            }}
