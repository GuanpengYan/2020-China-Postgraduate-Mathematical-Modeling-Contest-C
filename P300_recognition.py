import numpy as np
import keras
import matplotlib.pyplot as plt
import pandas as pd
from keras.layers import Dense, Dropout
from keras.layers import Input
from keras.layers import LSTM, SimpleRNN
from keras.models import Model, Sequential
from keras import optimizers
from sklearn import preprocessing
from keras.layers import Dense, Dropout, Flatten, Conv2D, MaxPooling2D
from pandas import DataFrame
from pandas import concat
from keras.optimizers import Adam
from scipy import signal
import xlrd
qishi = 40
jieshu = 150
steps = 1
sc = preprocessing.MaxAbsScaler()
workbook = xlrd.open_workbook("附件1-P300脑机接口数据/S1/S1_train_data.xlsx")
event = xlrd.open_workbook("附件1-P300脑机接口数据/S1/S1_train_event.xlsx")
worksheet = workbook.sheet_by_index(0)
eventsheet = event.sheet_by_index(0)
rownumber = eventsheet.nrows
list1 = []
list2 = []
for i in range(rownumber):
    numberdata = eventsheet.row_values(i)
    if numberdata[0]<100:
        position = int(numberdata[1])
        for i in range(40,150,steps):
            list2.append(worksheet.row_values(position+i))
        if numberdata[0]==1 or numberdata[0]==8:
            list1.append(1)
        else:
            list1.append(0)
        # print(list2)
worksheet = workbook.sheet_by_index(1)
eventsheet = event.sheet_by_index(1)
rownumber = eventsheet.nrows
for i in range(rownumber):
    numberdata = eventsheet.row_values(i)
    if numberdata[0]<100:
        position = int(numberdata[1])
        for i in range(40,150,steps):
            list2.append(worksheet.row_values(position+i))
        if numberdata[0]==1 or numberdata[0]==10:
            list1.append(1)
        else:
            list1.append(0)
worksheet = workbook.sheet_by_index(2)
eventsheet = event.sheet_by_index(2)
rownumber = eventsheet.nrows
for i in range(rownumber):
    numberdata = eventsheet.row_values(i)
    if numberdata[0]<100:
        position = int(numberdata[1])
        for i in range(40,150,steps):
            list2.append(worksheet.row_values(position+i))
        if numberdata[0]==2 or numberdata[0]==7:
            list1.append(1)
        else:
            list1.append(0)
worksheet = workbook.sheet_by_index(3)
eventsheet = event.sheet_by_index(3)
rownumber = eventsheet.nrows
for i in range(rownumber):
    numberdata = eventsheet.row_values(i)
    if numberdata[0]<100:
        position = int(numberdata[1])
        for i in range(40,150,steps):
            list2.append(worksheet.row_values(position+i))
        if numberdata[0]==2 or numberdata[0]==12:
            list1.append(1)
        else:
            list1.append(0)
worksheet = workbook.sheet_by_index(4)
eventsheet = event.sheet_by_index(4)
rownumber = eventsheet.nrows
for i in range(rownumber):
    numberdata = eventsheet.row_values(i)
    if numberdata[0]<100:
        position = int(numberdata[1])
        for i in range(40,150,steps):
            list2.append(worksheet.row_values(position+i))
        if numberdata[0]==3 or numberdata[0]==9:
            list1.append(1)
        else:
            list1.append(0)
worksheet = workbook.sheet_by_index(5)
eventsheet = event.sheet_by_index(5)
rownumber = eventsheet.nrows
for i in range(rownumber):
    numberdata = eventsheet.row_values(i)
    if numberdata[0]<100:
        position = int(numberdata[1])
        for i in range(40,150,steps):
            list2.append(worksheet.row_values(position+i))
        if numberdata[0]==3 or numberdata[0]==11:
            list1.append(1)
        else:
            list1.append(0)
worksheet = workbook.sheet_by_index(6)
eventsheet = event.sheet_by_index(6)
rownumber = eventsheet.nrows
for i in range(rownumber):
    numberdata = eventsheet.row_values(i)
    if numberdata[0]<100:
        position = int(numberdata[1])
        for i in range(40,150,steps):
            list2.append(worksheet.row_values(position+i))
        if numberdata[0]==4 or numberdata[0]==7:
            list1.append(1)
        else:
            list1.append(0)
worksheet = workbook.sheet_by_index(7)
eventsheet = event.sheet_by_index(7)
rownumber = eventsheet.nrows
for i in range(rownumber):
    numberdata = eventsheet.row_values(i)
    if numberdata[0]<100:
        position = int(numberdata[1])
        for i in range(40,150,steps):
            list2.append(worksheet.row_values(position+i))
        if numberdata[0]==4 or numberdata[0]==10:
            list1.append(1)
        else:
            list1.append(0)
worksheet = workbook.sheet_by_index(8)
eventsheet = event.sheet_by_index(8)
rownumber = eventsheet.nrows
for i in range(rownumber):
    numberdata = eventsheet.row_values(i)
    if numberdata[0]<100:
        position = int(numberdata[1])
        for i in range(40,150,steps):
            list2.append(worksheet.row_values(position+i))
        if numberdata[0]==5 or numberdata[0]==8:
            list1.append(1)
        else:
            list1.append(0)
worksheet = workbook.sheet_by_index(9)
eventsheet = event.sheet_by_index(9)
rownumber = eventsheet.nrows
for i in range(rownumber):
    numberdata = eventsheet.row_values(i)
    if numberdata[0]<100:
        position = int(numberdata[1])
        for i in range(40,150,steps):
            list2.append(worksheet.row_values(position+i))
        if numberdata[0]==5 or numberdata[0]==12:
            list1.append(1)
        else:
            list1.append(0)
worksheet = workbook.sheet_by_index(10)
eventsheet = event.sheet_by_index(10)
rownumber = eventsheet.nrows
for i in range(rownumber):
    numberdata = eventsheet.row_values(i)
    if numberdata[0]<100:
        position = int(numberdata[1])
        for i in range(40,150,steps):
            list2.append(worksheet.row_values(position+i))
        if numberdata[0]==6 or numberdata[0]==9:
            list1.append(1)
        else:
            list1.append(0)
worksheet = workbook.sheet_by_index(11)
eventsheet = event.sheet_by_index(11)
rownumber = eventsheet.nrows
for i in range(rownumber):
    numberdata = eventsheet.row_values(i)
    if numberdata[0]<100:
        position = int(numberdata[1])
        for i in range(40,150,steps):
            list2.append(worksheet.row_values(position+i))
        if numberdata[0]==6 or numberdata[0]==11:
            list1.append(1)
        else:
            list1.append(0)
data = np.array(list2)
b,a = signal.butter(4,[0.008,0.16],'bandpass')
filtedData = signal.filtfilt(b,a,data,axis=0)
data_sc = sc.fit_transform(data)
# df = pd.DataFrame(data)
data_sc_newformat = data_sc.reshape(720,110,20)
print(data.shape)
label = np.array(list1)
print(label)
print(label.shape)


data_sc_newformat = data_sc_newformat.reshape(720,110,20,1)
# print(data.shape)
model = Sequential()
model.add(Conv2D(32, kernel_size=(5, 5),
                 activation='relu',
                 input_shape=(110,20,1))) # 32个过滤器，过滤器大小是3×3，32×26×26
model.add(MaxPooling2D(pool_size=(2, 2)))
model.add(Conv2D(50, (3, 3), activation='relu')) #64×24×24
model.add(MaxPooling2D(pool_size=(2, 2)))# 向下取样
model.add(Dropout(0.25))
model.add(Flatten()) #降维：将64×12×12降为1维（即把他们相乘起来）
model.add(Dense(128, activation='relu'))
model.add(Dropout(0.5))
model.add(Dense(2, activation='softmax'))
#
# # # f = sc.fit_transform(f)
# # model = Sequential()
# # model.add(LSTM(100, input_shape=(data.shape[1], data.shape[2])))
# # model.add(Dense(1))
# sgd = optimizers.Adam(lr=0.001)
model.compile(loss='binary_crossentropy', optimizer='adam',metrics=['accuracy'])
history = model.fit(data_sc_newformat, label, epochs=500, batch_size=64,  shuffle=False)
# # rows = eventsheet.nrows()
# # for i in range(rows):
# #     rowdata = worksheet.row_values(i)

y= model.predict(data_sc_newformat)
print(y)

#     if rowdata[0] = 1:

