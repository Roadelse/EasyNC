# coding=utf-8

import sys
import re

class FCode_sim4spl:  # simplified for splitting lines
    qc_last = None

    def __init__(self, L: str, fformat, **kwargs):
        self.L = L.rstrip()
        self.fformat = fformat
        if self.fformat == 'fixed' and self.L.startswith('\t'):  # handle tab! docs.oracle.com/cd/E19957-01/805-4941/z40000a545d9/index.html, but with nonzero-digit test fails, just consider tab as 6 spaces by now @2022-12-15 13:34:04
            self.L = self.L.replace('\t', '      ', 1)
        self.L = self.L.replace('\t', '      ')
        self.L_std = self.L


    def removeComment(self):
        L: str = self.L_std
        # if self.L_std == "1009  FORMAT( / 5X, 'No BC''s in file ', A, ' for the following adv species:',":
        #     print(45649687654)
        if not L:
            return ""
        if FCode_sim4spl.qc_last is not None:
            inString = True
            qc = FCode_sim4spl.qc_last
            FCode_sim4spl.qc_last = None
        else:
            inString = False
            qc = ''

        StringQuotes = ('"', "'")
        pos = 0

        # handling "empty" lines
        if self.fformat == 'fixed' and self.L[0] in ('c', 'C', '*', '!'):  # * and ! are updated @2023-01-05 09:56:03
            self.L_std = ""
            return
        if self.L[0] == '!':
            self.L_std = ""
            return

        commentChar = '!'

        while pos < len(L):
            if L[pos] == commentChar and not inString:  # encounter commentChar and break
                break

            if inString:  # skip string
                if L[pos] == qc:
                    if pos + 1 < len(L) and L[pos + 1] == qc:
                        pos += 2
                        continue   # skip "''", i.e., actual quote in stringwwwwwwwwww
                    else:
                        inString = False
                        qc = ''
            else:
                if L[pos] in StringQuotes:  # encounter string
                    qc = L[pos]
                    inString = True
            pos += 1

        if inString:
            self.L_std = L.rstrip()  # strip correct??

            self.LC_next = True
            FCode_sim4spl.qc_last = qc
            return
        else:
            self.L_std = L[:pos].rstrip()
            self.comment = L[pos+1:].strip()
            return


    @classmethod
    def check_line_length_and_split(cls, L: str, fformat: str = 'fixed', maxCol: int = 132):
        fc = cls(L, fformat)
        cls.qc_last = None

        fc.L_std = fc.L  # for convenience @2023-05-25 17:15:42
        fc.removeComment()
        # print(len(fc.L_std), fc.L_std)

        if len(fc.L_std) > maxCol:
            # print(1111111111111111)
            i = maxCol - 5
            while i > 0:
                if fc.L_std[i] in (",", "("):
                    if fformat == 'fixed':
                        L0 = fc.L_std[:i+1]
                        L1 = "     & " + fc.L_std[i+1:] + " ! " + fc.comment
                    else:
                        L0 = fc.L_std[:i+1] + " &"
                        L1 = "    " + fc.L_std[i+1:] + " ! " + fc.comment
                    return L0 + "\n" + cls.check_line_length_and_split(L1, fformat, maxCol)  # recursive to handle toooooo long lines
                i -= 1
            raise UnexpectedCondition("no commas!")
        else:
            return L

