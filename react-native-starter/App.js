import React from "react";
import
  { Dimensions
  , TextInput
  , StyleSheet
  , Text
  , View
  } from "react-native";

export default class App extends React.Component {
  state = { text: "" }

  config = { placeholder: "type whatever you want here" }

  onChangeText(text) {
    return this.setState({...this.state, text })
  }

  render() {
    return (
      <View style={styles.container}>
        <Text> {this.state.text.split('').reverse().join('')} </Text>
        <TextInput
          style={{
              borderWidth: 1
            , borderColor: "gray"
            , padding: 5
          }}
          onChangeText={this.onChangeText.bind(this)}
          placeholder={this.config.placeholder}
          value={this.state.text}
        />
      </View>
    );
  }
}

const styles = StyleSheet.create({
  container: {
    backgroundColor: '#fff',
    flex: 1,
    alignItems: 'center',
    justifyContent: 'center',
  },
});
