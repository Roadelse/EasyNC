# coding=utf-8

import sys
import re

class FCode_sim4spl:  # simplified for splitting lines

    @classmethod
    def render_str_comment_mask(cls, L: str, fformat='free'):
        """
        This class function aims to render a mask for all characters in the L, with 1 representing in string, 2 representing comment,  and 0 representing neither
        """
        if not L:
            return []

        scMask = [0 for _ in L]  # string, comment mask

        if fformat == 'fixed' and L[0] in ('c', 'C', '!', '*'):
            scMask = [1 for _ in L]
            return scMask

        inString = False
        qc = ''

        StringQuotes = ('"', "'")
        pos = 0

        commentChar = '!'

        while pos < len(L):
            if L[pos] == commentChar and not inString:  # find comment, set all following mask to 2 and return
                for i in range(pos, len(L)):
                    scMask[i] = 2
                return scMask

            if inString:  # skip string
                scMask[pos] = 1
                if L[pos] == qc:
                    if pos + 1 < len(L) and L[pos + 1] == qc:
                        scMask[pos] = 1
                        scMask[pos+1] = 1
                        pos += 2
                        continue   # skip "''", i.e., actual quote in the string
                    inString = False
                    qc = ''
            else:
                if L[pos] in StringQuotes:  # encounter string
                    qc = L[pos]
                    scMask[pos] = 1
                    inString = True
            pos += 1

        return scMask

    @classmethod
    def test_render_str_comment_mask(cls):
        L = "      write(*,*) '234' ! kasdb"
        assert cls.render_str_comment_mask(L, 'fixed') == [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 2, 2, 2, 2, 2, 2, 2], f'now is {cls.render_str_comment_mask(L, "fixed")}'
        L = """print *, 'nani''haha''', ! 'xcna ad9''s"'""'""' """
        assert cls.render_str_comment_mask(L, 'free') == [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]
        print('>>>>> \033[33mSucceed to test render_str_comment_mask \033[m<<<<<')


    @classmethod
    def check_line_length_and_split(cls, L: str, fformat: str = 'fixed', maxCol: int = 132):
        # fc = cls(L)
        # cls.qc_last = None

        # fc.L_std = fc.L  # for convenience @2023-05-25 17:15:42
        # fc.removeComment()
        # print(len(fc.L_std), fc.L_std)
        scMask = cls.render_str_comment_mask(L, fformat)
        L_noC = L
        for i in range(len(L)):
            if scMask[i] == 2:
                L_noC = L[:i]
                break

        if len(L_noC) > maxCol:
            i = maxCol - 20
            while i > 0:
                if L_noC[i] in (",", "("):
                    if fformat == 'fixed':
                        L0 = L_noC[:i+1]
                        L1 = "     & " + L_noC[i+1:]
                    else:
                        L0 = L_noC[:i+1] + " &"
                        L1 = "    " + ('&' if scMask[i] == 1 else '') + L_noC[i+1:]
                    return L0 + "\n" + cls.check_line_length_and_split(L1, fformat, maxCol)  # recursive to handle toooooo long lines
                i -= 1
            raise UnexpectedCondition(f"no commas! {L_noC}")
        else:
            return L

    @classmethod
    def test_check_line_length_and_split(cls):
        L = "            call assert(size(dimnames)+1 .eq. rank_ncv, 'Error in easyO_string_7d, argument<dimnames> should have the same size as rank_ncv (for string, needs +1)') !123"
        L_split = cls.check_line_length_and_split(L, 'free', 132)
        assert L_split == "            call assert(size(dimnames)+1 .eq. rank_ncv, 'Error in easyO_string_7d, &\n    & argument<dimnames> should have the same size as rank_ncv (for string, needs +1)') "

        print('>>>>> \033[33mSucceed to test check_line_length_and_split \033[m<<<<<')



if __name__ == '__main__':
    FCode_sim4spl.test_render_str_comment_mask()
    FCode_sim4spl.test_check_line_length_and_split()