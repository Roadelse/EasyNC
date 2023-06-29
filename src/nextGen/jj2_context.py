# coding=utf-8


def get_rank_from_nd(n: int):
    if n == 0:
        return ''
    
    colons = [':' for _ in range(n)]
    rst_str = '(' + ','.join(colons) + ')'

    return rst_str


ctt = {'get_rank_from_nd' : get_rank_from_nd}
