#!/usr/bin/env python3
import os
from io import BytesIO
from pathlib import Path
from optparse import OptionParser
from struct import unpack, unpack_from, calcsize

RGBDS_REVISION_LOW = 6
RGBDS_REVISION_HIGH = 9

WRAM0 = 0; VRAM = 1; ROMX = 2; ROM0 = 3; HRAM = 4; WRAMX = 5; SRAM = 6; OAM = 7
SYM_LOCAL = 0; SYM_IMPORT = 1; SYM_EXPORT = 2
PATCH_BYTE = 0; PATCH_LE_WORD = 1; PATCH_LE_LONG = 2; PATCH_JR = 3

rpnPlus = 0x00; rpnMinus = 0x01; rpnTimes = 0x02; rpnDiv = 0x03; rpnMod = 0x04; rpnNegate = 0x05; rpnExponent = 0x06; rpnOr = 0x10;
rpnAnd = 0x11; rpnXor = 0x12; rpnComplement = 0x13; rpnBoolAnd = 0x21; rpnBoolOr = 0x22; rpnBoolNeg = 0x23; rpnEqual = 0x30; rpnNotEqual = 0x31;
rpnGreater = 0x32; rpnLess = 0x33; rpnGreaterEqual = 0x34; rpnLessEqual = 0x35; rpnShl = 0x40; rpnShr = 0x41; rpnBankSymbol = 0x50; rpnBankSection = 0x51;
rpnCurrentBank = 0x52; rpnSizeOfSection = 0x53; rpnStartOfSection = 0x54; rpnHramCheck = 0x60; rpnRstCheck = 0x61; rpnInteger = 0x80; rpnSymbol = 0x81

class ByteStream(BytesIO):
    def read_record(self, fmt):
        return unpack(fmt, self.read(calcsize(fmt)))

    def read_null_term(self):
        res = []
        while True:
            c = self.read(1)
            if (len(c) == 0): break
            if c[0] == 0: break
            res.append(c.decode('ascii'))
        return ''.join(res)

    def read_array(self, fmt, count):
        len = calcsize(fmt)
        res = []
        for j in range(count):
            res.append(unpack(fmt, self.read(len)))
        return res

class RGBObject(object):
    def __init__(self, data):
        self.Symbols  = []
        self.Sections = []
        self.Nodes = []

        stream = ByteStream(data)

        self.id, self.rev = stream.read_record('<4si')
        if (self.id != b'RGB9') or not (RGBDS_REVISION_LOW <= self.rev <= RGBDS_REVISION_HIGH):
            raise Exception("RGBDS Object version mismatch! Expected: {:d} received: {:d}".format(RGBDS_REVISION, self.rev))

        nSymbols, nSections, nNodes = stream.read_record('<iii')

        # read Nodes
        for i in range(nNodes):
            parentid, parentlineno, nodetype = stream.read_record('<iiB')
            if (nodetype != 0):
                name, iter = stream.read_null_term(), None
            else:
                name, iter = None, stream.read_array('<i', *stream.read_record('<i'))

            self.Nodes.append({'ParentID': parentid, 'ParentLineNo': parentlineno, 'NodeType': nodetype, 'Name': name, 'Iter': iter})

        # read Symbols
        for i in range(nSymbols):
            name = stream.read_null_term()
            symtype, = stream.read_record('B')
            if ((symtype & 0x7f) != SYM_IMPORT):
                sourcefile, linenum, sectionid, value = stream.read_record('<iiii')
                self.Symbols.append({'Id': i, 'Name': name, 'SymType': symtype, 'SourceFile': sourcefile, 'LineNum': linenum, 'SectionId': sectionid, 'Value': value})
            else:
                self.Symbols.append({'Id': i, 'Name': name, 'SymType': symtype})

        # read Sections
        for i in range(nSections):
            name = stream.read_null_term()
            size, secttype, org, bank, align, ofs = stream.read_record('<iBiiBi')

            sect = {'Id': i, 'Name': name, 'Size': size, 'SectType': secttype, 'Org': org, 'Bank': bank, 'Align': align, 'Ofs': ofs}
            self.Sections.append(sect)

            if (sect['SectType'] in [ROM0, ROMX]):
                sect['Data'] = stream.read(size)
                npatches, = stream.read_record('<i')
                sect['Patches'] = patches = []
                for j in range(npatches):
                    sourcefile, linenum, ofs, pcsectionid, pcoffset, patchtype, rpnsize = stream.read_record('<iiiiiBi')
                    rpn = self.decode_rpn(stream.read(rpnsize))
                    patches.append({'SourceFile': sourcefile, 'LineNum': linenum, 'Offset': ofs, 'PCSectionId': pcsectionid, 'PCOffset': pcoffset, 'PatchType': patchtype, 'RPN': rpn})

    def decode_rpn(self, data):
        result = []
        stream = ByteStream(data)
        while True:
            tag = stream.read(1)
            if len(tag) == 0:
                break
            else:
                tag = tag[0]
            itm = {'Tag': tag}
            if tag == rpnBankSymbol:
                itm['BankSymbol'] = stream.read_record('<i')
            elif tag == rpnBankSection:
                itm['BankSection'] = stream.read_null_term()
            elif tag == rpnSizeOfSection:
                itm['SizeOfSection'] = stream.read_null_term()
            elif tag == rpnStartOfSection:
                itm['StartOfSection'] = stream.read_null_term()
            elif tag == rpnInteger:
                itm['IntValue'], = stream.read_record('<i')
            elif tag == rpnSymbol:
                itm['SymbolId'], = stream.read_record('<i')
            result.append(itm)
        return result

    def find_patch(self, section, index):
        for patch in section['Patches']:
            if (patch['Offset'] == index):
                return (True, patch)
        return (False, {})

    def log(self):
        print("ID: {:s} REV: {:d}".format(self.id.decode('ascii'), self.rev))
        print(self.Nodes)
        print(self.Symbols)
        print(self.Sections)



def lo(v): return v & 0xff
def hi(v): return (v >> 8) & 0xff

def main(argv=None):
    parser = OptionParser("Usage: rgb2sdas.py [options] INPUT_FILE_NAME.OBJ")
    parser.add_option("-o", '--out',        dest='outfilename',                                          help='output file name')

    parser.add_option("-b", '--bank',       dest='default_bank',                       default="0",      help='BANK number (default: 0)')
    parser.add_option("-c", '--codeseg',    dest='CODESEG',                            default="_CODE",  help='CODE segment name (default: "_CODE")')
    parser.add_option("-r", '--rename',     dest='rename',                                               help='rename symbol: old=new')
    parser.add_option("-e", '--export-all', dest='export_all',   action="store_true",  default=False,    help='export all symbols')
    parser.add_option("-m", '--target',     dest='target',                             default="sm83",   help='target platform (default: "sm83")')

    (options, args) = parser.parse_args()

    if (len(args) == 0):
        parser.print_help()
        parser.error("Input file name required\n")

    infilename = Path(args[0])

    if (options.outfilename == None):
        outfilename = infilename.with_suffix('.o')
    else:
        outfilename = Path(options.outfilename)

    if (options.rename != None):
        old_sym, new_sym = str(options.rename).split('=')
    else:
        old_sym = new_sym = ''

    with open(str(infilename), mode="rb") as f:
        obj = RGBObject(f.read())

    try:
        with open(str(outfilename), "wb") as f:
            idx = 0
            # pass 1: all imports first
            for symbol in obj.Symbols:
                if ((symbol['SymType'] & 0x7f) == SYM_IMPORT):
                    symbol['No'] = idx
                    idx += 1
                elif ((symbol['SymType'] & 0x7f) == SYM_EXPORT):
                    symbol['No'] = -1
                    section = obj.Sections[symbol['SectionId']]
                    if (section['SectType'] == ROMX):
                        symbol['BankAlias'] = (max(int(options.default_bank), section['Bank']) > 0)
                        symbol['BankValue'] = max(int(options.default_bank), section['Bank'])
                        if (symbol['BankAlias']):
                            idx += 1
                else:
                    symbol['No'] = -1

            # pass 2: all other (export local only when forced)
            for symbol in obj.Symbols:
                if ((symbol['SymType'] & 0x7f) == SYM_LOCAL):
                    if (options.export_all):
                        symbol['No'] = idx
                        idx += 1
                elif ((symbol['SymType'] & 0x7f) == SYM_IMPORT):
                    pass
                elif ((symbol['SymType'] & 0x7f) == SYM_EXPORT):
                    if ((len(old_sym) != 0) and (symbol['Name'] == old_sym)):
                        symbol['Name'] = new_sym
                    symbol['No'] = idx
                    idx += 1
                else:
                    raise Exception('Unsupported symbol type: {:d}'.format(symbol['SymType'] & 0x7f))

            # output object header
            f.write(bytes('XL3\n', 'ascii'))
            f.write(bytes('H {:X} areas {:X} global symbols\n'.format(len(obj.Sections), idx), 'ascii'))
            f.write(bytes('M {:s}\n'.format(infilename.name.replace('.', '_')), 'ascii'))
            f.write(bytes('O -m{:s}\n'.format(options.target), 'ascii'))

            # output all imported symbols
            for symbol in obj.Symbols:
                if ((symbol['SymType'] & 0x7f) == SYM_IMPORT):
                    f.write(bytes('S {:s} Ref{:06X}\n'.format(symbol['Name'].replace('.', '____'), 0), 'ascii'))
                elif ((symbol['SymType'] & 0x7f) == SYM_EXPORT) and (symbol.setdefault('BankAlias', False)):
                    f.write(bytes('S b{:s} Ref{:06X}\n'.format(symbol['Name'].replace('.', '____'), symbol['BankValue']), 'ascii'))

            # output all sections and other symbols
            for section in obj.Sections:
                if (section['Org'] == -1):
                    if (section['SectType'] == ROM0):
                        f.write(bytes('A {:s} size {:X} flags 0 addr 0\n'.format(options.CODESEG, section['Size']), 'ascii'));
                    elif (section['SectType'] == ROMX):
                        if (int(options.default_bank) == 0):
                            f.write(bytes('A {:s} size {:X} flags 0 addr 0\n'.format(options.CODESEG, section['Size']), 'ascii'));
                        else:
                            f.write(bytes('A _CODE_{:d} size {:X} flags 0 addr 0\n'.format(max(int(options.default_bank), section['Bank']), section['Size']), 'ascii'));
                    else:
                        f.write(bytes('A _DATA size {:X} flags 0 addr 0\n'.format(section['Size']), 'ascii'));
                else:
                    raise Exception('Absolute sections currently unsupported: {:s}'.format(section['Name']))

                for symbol in obj.Symbols:
                    if (symbol['SectionId'] == section['Id']):
                        if (((symbol['SymType'] & 0x7f) != SYM_IMPORT) and (symbol['No'] >= 0)):
                            f.write(bytes('S {:s} Def{:06X}\n'.format(symbol['Name'], symbol['Value']).replace('.', '____'), 'ascii'))


            # convert object itself
            for section in obj.Sections:
                if (section['SectType'] != ROM0) and (section['SectType'] != ROMX):
                    continue

                data = section.setdefault('Data', [])
                if (len(data) == 0):
                    continue;

                read_position = 0
                while read_position < len(data):
                    PC = read_position + (section['Org'] if (section['Org'] != -1) else 0)

                    res, patch = obj.find_patch(section, read_position)
                    if (res):
                        RPN = patch['RPN']
                        if (patch['PatchType'] == PATCH_BYTE):
                            if (((len(RPN) == 3) and ((RPN[1]['Tag'] != rpnInteger) or (RPN[2]['Tag'] != rpnAnd))) or
                                ((len(RPN) == 5) and ((RPN[1]['Tag'] != rpnInteger) or (RPN[2]['Tag'] != rpnShr) or (RPN[3]['Tag'] != rpnInteger) or (RPN[4]['Tag'] != rpnAnd))) or
                                (not len(RPN) in [3, 5])):
                                raise Exception('Unsupported RPN expression in byte patch')

                            symbol = obj.Symbols[RPN[0]['SymbolId']]
                            if len(RPN) == 3:
                                f.write(bytes('T {:02X} {:02X} 00 {:02X} {:02X} 00\n'.format(lo(PC), hi(PC), lo(symbol['Value']), hi(symbol['Value'])), 'ascii'))
                                f.write(bytes('R 00 00 {:02X} {:02X} 09 03 {:02X} {:02X}\n'.format(lo(section['Id']), hi(section['Id']), lo(symbol['SectionId']), hi(symbol['SectionId'])), 'ascii'))
                            elif len(RPN) == 5:
                                f.write(bytes('T {:02X} {:02X} 00 {:02X} {:02X} 00\n'.format(lo(PC), hi(PC), lo(symbol['Value']), hi(symbol['Value'])), 'ascii'))
                                f.write(bytes('R 00 00 {:02X} {:02X} 89 03 {:02X} {:02X}\n'.format(lo(section['Id']), hi(section['Id']), lo(symbol['SectionId']), hi(symbol['SectionId'])), 'ascii'))
                            read_position += 1
                        elif (patch['PatchType'] == PATCH_LE_WORD):
                            if (((len(RPN) == 3) and ((RPN[1]['Tag'] != rpnInteger) or (RPN[2]['Tag'] != rpnPlus))) or
                                (not len(RPN) in [1, 3])):
                                raise Exception('Unsupported RPN expression in word patch')

                            symbol = obj.Symbols[RPN[0]['SymbolId']]
                            value_to_write = symbol['Value']

                            if (RPN[-1]['Tag'] == rpnPlus):
                                value_to_write += RPN[1]['IntValue']

                            if (symbol['SymType'] == SYM_IMPORT):
                                if (symbol['No'] < 0):
                                    raise Exception('Trying to reference eliminated symbol');
                                f.write(bytes('T {:02X} {:02X} 00 {:02X} {:02X}\n'.format(lo(PC), hi(PC), lo(value_to_write), hi(value_to_write)), 'ascii'))
                                f.write(bytes('R 00 00 {:02X} {:02X} 02 03 {:02X} {:02X}\n'.format(lo(section['Id']), hi(section['Id']), lo(symbol['No']), hi(symbol['No'])), 'ascii'))
                            else:
                                f.write(bytes('T {:02X} {:02X} 00 {:02X} {:02X}\n'.format(lo(PC), hi(PC), lo(value_to_write), hi(value_to_write)), 'ascii'))
                                f.write(bytes('R 00 00 {:02X} {:02X} 00 03 {:02X} {:02X}\n'.format(lo(section['Id']), hi(section['Id']), lo(symbol['SectionId']), hi(symbol['SectionId'])), 'ascii'))
                            read_position += 2
                        elif (patch['PatchType'] == PATCH_JR):
                            if (len(RPN) != 1):
                                raise Exception('Unsupported RPN expression in JR patch')

                            symbol = obj.Symbols[RPN[0]['SymbolId']]
                            f.write(bytes('T {:02X} {:02X} 00 {:02X}\n'.format(lo(PC), hi(PC), symbol['Value'] - read_position - 1), 'ascii'))
                            f.write(bytes('R 00 00 {:02X} {:02X}\n'.format(lo(section['Id']), hi(section['Id'])), 'ascii'))
                            read_position += 1
                        else:
                            raise Exception('Unsupported patch type: {:d} Section: {:s}'.format(patch['PatchType'], section['Name']))
                    else:
                        f.write(bytes('T {:02X} {:02X} 00 {:02X}\n'.format(lo(PC), hi(PC), data[read_position]), 'ascii'))
                        f.write(bytes('R 00 00 {:02X} {:02X}\n'.format(lo(section['Id']), hi(section['Id'])), 'ascii'))
                        read_position += 1

        print('RGB2SDAS: object {:s} was successfully converted to {:s}'.format(infilename.name, outfilename.name))
    except Exception as e:
        os.remove(str(outfilename))
        raise e

if __name__=='__main__':
    main()
