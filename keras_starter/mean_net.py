from keras.models import Sequential
from keras.layers import Dense

import numpy as np

model = Sequential()

# 10.5, 5, 9.5, 12 => 18.5

model.add(Dense(8, activation="relu", input_dim=4)) # 1st hidden layer
model.add(Dense(16, activation="relu")) # 2nd hidden layer
model.add(Dense(8, activation="relu")) # 3rd hidden layer
model.add(Dense(1, activation="linear")) # output layer

model.compile(
    optimizer='adam',
    loss='mean_squared_error'
)

model.summary()

x_train = np.array([
    [1,2,3,4],
    [4,6,1,2],
    [10,9,10,11],
    [10,12,9,13],
    [99,100,101,102],
    [105,111,109,102]
])

y_train = np.array([
    [2.5],
    [3.25],
    [10.0],
    [11.0],
    [100.5],
    [106.75]
])

# perm = np.random.permutation(y_train.size)
# x_train = x_train[perm]
# y_train = y_train[perm]

x_val = np.array([
  [1.5, 4, 3, 2.5],
  [10, 14, 11.5, 12],
  [111, 99, 105, 107]
])

y_val = np.array([
  [2.75],
  [11.875],
  [105.5],
])

model.fit(
    x_train,
    y_train,
    batch_size=2,
    epochs=100,
    verbose=1,
    # validation_split=0.2,
    validation_data=[x_val, y_val]
)

model.save("mean.net")

x_test = np.array([
  [2, 5, 4.5, 1],
  [9, 16, 11, 10.5],
  [100, 95, 99, 102]
])

y_test = np.array([
  [3.125],
  [11.625],
  [99.0],
])

output = model.evaluate(x_test, y_test)

print("")
print("=== Evaluation ===")
print(model.metrics_names)
print(output)

x_predict = np.array([
    [1.5, 2, 3.5, 4],
    [1000, 2000, 3000, 4000]
])

prediction = model.predict(x_predict)

print("")
print("Expected: [[2.75], [2500]]")
print("Actual:", prediction)
