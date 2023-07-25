# coding=utf-8


def get_rank_from_nd(n: int):
    if n == 0:
        return ''
    
    colons = [':' for _ in range(n)]
    rst_str = '(' + ','.join(colons) + ')'

    return rst_str

#dimsT2_enc(1,1):dimsT2_enc(2,1)
def get_dimsT2_enc_arrayBound(n: int):
    ulb_by_dims = [f'dimsT2_enc(1,{i}):dimsT2_enc(2,{i})' for i in range(1, n+1)]
    rst_str = ','.join(ulb_by_dims)

    return rst_str

def get_dimsT_enc_arrayShape(n: int):
    ulb_by_dims = [f'dimsT_enc({i})' for i in range(1, n+1)]
    rst_str = ','.join(ulb_by_dims)

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
       'get_dimsT_enc_arrayShape' : get_dimsT_enc_arrayShape,
       'get_dimsT2_enc_arrayBound' : get_dimsT2_enc_arrayBound, 
       'get_b_shape' : get_b_shape}
