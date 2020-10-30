import sys
sys.path.insert(1, '../../../packages/python/encryptFileColumns')

import encryptFileColumns as efc
import os

rootInputFileDirectory = r'P:\FMHSfiles\SCIENCE\CheckWHO\data\raw\password_protected_source'
rootOutputFileDirectory = r'P:\FMHSfiles\SCIENCE\CheckWHO\data\derived\encrypted_id_source'

adhbInputFileDirectory = os.path.join(rootInputFileDirectory)

identifiableColNames = ['NHI', 'nhi', 'PRIM_HCU', 'HCU_ID']

adhbInputFileDirectory = os.path.join(rootInputFileDirectory, 'adhb')
adhbInputFileName = '19123 Level 8 Theatre Events.xlsx'
adhbInputWorksheetName = 'Data'
adhbOutputFileDirectory = os.path.join(rootOutputFileDirectory, 'adhb')

mohInputFileDirectory = os.path.join(rootInputFileDirectory, 'moh')
nap_mohInputFileName = 'nap0917.zip'
pus_mohInputFileName = 'pus10784.zip'
nap_mohArchiveFileName = 'nap0917.txt'
pus_mohArchiveFileNames = ['pus10784_cohort.txt', 'pus10784_diags.txt', 'pus10784_events.txt']
pus_mohArchiveFileNames = ['pus10784_cohort.txt', 'pus10784_diags.txt', 'pus10784_events.txt']
# pus_mohArchiveFileNames = 'pus10784_diags.txt'
pus_mohArchiveFileNames = 'pus10784_events.txt'
mohOutputFileDirectory = os.path.join(rootOutputFileDirectory, 'moh')

# adhbPassword = input('Please provide ADHB password: ')
mohPassword = input('Please provide MOH password: ')
outputEncryptionPassword = input('Please provide output encryption password: ')

# # Encrypt ADHB data
# efc.encryptFileColumns(
#     inputFileName=adhbInputFileName,
#     inputFileDirectory=adhbInputFileDirectory,
#     inputFilePassword=adhbPassword,
#     inputFileWorksheetOrArchiveFileNameOrIndex=adhbInputWorksheetName,
#     outputFileDirectory=adhbOutputFileDirectory,
#     columns=identifiableColNames,
#     outputEncryptionPassword=outputEncryptionPassword,
#     saveFormat='csv')

# Encrypt MOH data
# efc.encryptFileColumns(
#     inputFileName=nap_mohInputFileName,
#     inputFileDirectory=mohInputFileDirectory,
#     inputFilePassword=mohPassword,
#     outputFileDirectory=mohOutputFileDirectory,
#     columns=identifiableColNames,
#     outputEncryptionPassword=outputEncryptionPassword,
#     saveFormat='csv',
#     sep='|')

efc.encryptFileColumns(
    inputFileName=pus_mohInputFileName,
    inputFileDirectory=mohInputFileDirectory,
    inputFilePassword=mohPassword,
    inputFileWorksheetOrArchiveFileNameOrIndex=pus_mohArchiveFileNames,
    outputFileDirectory=mohOutputFileDirectory,
    columns=identifiableColNames,
    outputEncryptionPassword=outputEncryptionPassword,
    saveFormat='csv',
    sep='|')
