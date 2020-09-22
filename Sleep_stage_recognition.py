import numpy as np
import keras
from keras.models import Sequential
from keras.layers import Dense, Dropout, Flatten
from keras.layers import Conv2D, MaxPooling2D
from keras.optimizers import SGD
from keras.utils import plot_model
import xlrd
from sklearn import preprocessing
from sklearn import svm
import matplotlib.pyplot as plt
workbook = xlrd.open_workbook("附件2-睡眠脑电数据.xlsx")
# names=workbook.sheet_names()
# print(names)
datalist = []
for i in range(0,5):
    worksheet = workbook.sheet_by_index(i)
    nrows = worksheet.nrows
    for m in range(1,nrows):
        datalist.append(worksheet.row_values(m))
data = np.array(datalist)
x = np.array(data[:,1:])
x = np.delete(x,[15,16],axis=1)
min_max_scaler = preprocessing.MaxAbsScaler()
x = min_max_scaler.fit_transform(x)
print(np.std(x,axis=0))
y = np.array(data[:,0])
y_int = [ int(c-2) for c in y ]
y_int = np.array(y_int)
print(y_int)
np.random.seed(116)
np.random.shuffle(x)
np.random.seed(116)
np.random.shuffle(y_int)
x_train = x[:600,:]
print(x_train.shape)
# x_train = np.delete(x_train,2,axis=1)
y_train = y_int[:600]
x_test = x[600:,:]
# x_test = np.delete(x_test,2,axis=1)
y_test = y_int[600:]

model = Sequential()
model.add(Dense(128,activation='relu'))
# model.add(Dense(512,activation='relu'))
model.add(Dense(256,activation='relu'))
model.add(Dense(128,activation="relu"))
model.add(Dense(5,activation='softmax'))
adam = keras.optimizers.Adam(lr=0.0001)
model.compile(optimizer=adam,loss=keras.losses.SparseCategoricalCrossentropy(from_logits=False),
              metrics=['sparse_categorical_accuracy'])
hist = model.fit(x_train,y_train,epochs=300,batch_size=32,validation_data=(x_test,y_test),validation_freq=1)
acc = hist.history['sparse_categorical_accuracy']
val_acc = hist.history['val_sparse_categorical_accuracy']
loss = hist.history['loss']
val_loss = hist.history['val_loss']
epochs = range(len(acc))
plt.plot(epochs, acc, 'b', label='Training acc')  # 'bo'为画蓝色圆点，不连线
plt.plot(epochs, val_acc, 'r', label='Validation acc')
plt.title('Training and validation accuracy')
plt.legend()  # 绘制图例，默认在右上角

plt.figure()

plt.plot(epochs, loss, 'b', label='Training loss')
plt.plot(epochs, val_loss, 'r', label='Validation loss')
plt.title('Training and validation loss')
plt.legend()

plt.show()

clf = svm.SVC(C=100, kernel='rbf', gamma=220, decision_function_shape='ovo')
clf.fit(x_train, y_train)
print('训练集正确率：',clf.score(x_train, y_train))  # 精度
y_hat = clf.predict(x_train)
print ('验证集正确率：',clf.score(x_test, y_test))
y_hat = clf.predict(x_test)


