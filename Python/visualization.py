import numpy as np
from scipy import signal
import  matplotlib.pyplot as plt
import IPython
import glob
import os
import copy as copy

def writeStateSpaceFresp(sourceFileNames, topFolder, ref = True,ts = 0.01, NUM_MUSCLES = 7, NUM_OUTPUTS = 6):
	fs = 1 / ts
	freq = np.linspace(0, np.pi, 1000)
	header = ''
	ss_matrix = {}
	for dataFileName in sourceFileNames:
		for mat_name in ['A', 'B', 'C', 'D']:
			if ref == True:
				f_name = os.path.join(topFolder , "{}_Meas_{}".format(dataFileName[:-4], mat_name))
			elif ref == False:
				f_name = os.path.join(topFolder , "{}_Ref_{}".format(dataFileName[:-4], mat_name))
			else:
				raise ValueError('Wrong input, ref must be either True or False')
			ss_matrix[mat_name] = np.transpose(np.loadtxt(f_name, delimiter = ',', unpack = True))
		for muscleIndex in range(0, NUM_MUSCLES):
			for outputIndex in range(0, NUM_OUTPUTS):
				thisB = ss_matrix["B"][:,muscleIndex:muscleIndex + 1]
				thisC = ss_matrix["C"][outputIndex:outputIndex + 1,:]
				thisD = [[ss_matrix["D"][outputIndex,muscleIndex]]]
				thisSys = signal.StateSpace(ss_matrix["A"],thisB,thisC,thisD,dt = ts)
				thisSysTF = signal.TransferFunction(thisSys)
				w, mag, phase = thisSysTF.bode(freq)
				dataWriteThisSys = (np.transpose(np.concatenate(([w / 2 / np.pi],[mag],[phase]),0)))

				for headerText in ['freq','mag','phase']:
					header += '{}_i{}o{},'.format(headerText, muscleIndex, outputIndex)

				if 'dataWrite' in locals():
					dataWrite = np.concatenate((dataWrite,dataWriteThisSys),1)
				else:
					dataWrite = copy.copy(dataWriteThisSys)
		header = header[:-1] + '\n'
		writeFileName = os.path.join(topFolder , '{}_Meas_fresp.csv'.format(dataFileName[:-4]))
		fileW = open(writeFileName, 'w')
		fileW.write(header)
		fileW.close()
		fileW = open(writeFileName, 'ab')
		np.savetxt(fileW, dataWrite, delimiter=",")
		fileW.close()
		del dataWrite
		header = ''



readFolder = '/Users/kian/Documents/archive/publication/postureDependency/frontiers2017_data/'
writeFolder = '/Users/kian/Documents/archive/publication/postureDependency/frontiers2017/dynamicAnalysisResults/'
dataFileNameWithFolder = glob.glob(readFolder + '/*timeseries.csv', recursive = True)
dataFileNames = [os.path.basename(x) for x in dataFileNameWithFolder]
IPython.embed()
writeStateSpaceFresp(dataFileNames, writeFolder, ref = True)
writeStateSpaceFresp(dataFileNames, writeFolder, ref = False)

