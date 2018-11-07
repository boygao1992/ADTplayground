from keras.models import Sequential
from keras.layers import Dense, Activation

import numpy as np


# < 50 = 0 (low)
# > 50 = 1 (high)
model = Sequential()

model.add(Dense(8, activation='relu', input_dim=4))
model.add(Dense(16, activation='relu'))
model.add(Dense(32, activation='relu'))
model.add(Dense(16, activation='relu'))
model.add(Dense(8, activation='relu'))
model.add(Dense(1, activation='sigmoid')) # y in (0, 1)

model.compile(
    optimizer='adam',
    loss='binary_crossentropy',
    metrics=['accuracy']
)

data = np.genfromtxt('high_low.csv', delimiter=',')


# x_train = np.array([
#   [1, 2, 3, 4],
#   [4, 6, 1, 2],
#   [10, 9, 10, 11],
#   [101, 95, 89, 111],
#   [99, 100, 101, 102],
#   [105, 111, 109, 102]
# ])

# y_train = np.array([
#   [0],
#   [0],
#   [0],
#   [1],
#   [1],
#   [1]
# ])

x_train = data[1:, :4]
y_train = data[1:, 4]

# x_val = np.array([
#   [1.5, 4, 3, 2.5],
#   [10, 14, 11.5, 12],
#   [111, 99, 105, 107]
# ])

# y_val = np.array([
#   [0],
#   [0],
#   [1],
# ])

model.fit(
    x_train,
    y_train,
    epochs=100,
    batch_size=2,
    verbose=1,
    # validation_data=(x_val, y_val),
    validation_split=0.2
)

x_predict = np.array([
    [10, 25, 14, 9],
    [102, 100, 75, 90]
])

output = model.predict(x_predict)
prediction = model.predict_classes(x_predict)

print("")
print(output)
print(prediction)
