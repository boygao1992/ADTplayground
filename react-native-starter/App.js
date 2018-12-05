import React from "react";
import
  { Dimensions
  , FlatList
  , TextInput
  , StyleSheet
  , Text
  , View
  } from "react-native";

export default class App extends React.Component {
  state = { text: "", data: [] }

  config = { placeholder: "type whatever you want here" }

  onChangeText(text) {
    return this.setState({...this.state, text })
  }

  componentDidMount(){
    fetch("http://10.0.2.2:8080/gems")
    .then( res => res.json() )
    .then( data => {
      this.setState({...this.state, data })
    })
    .catch( err => {
      console.warn(err)
    })
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
        <FlatList
          data={this.state.data}
          renderItem={({ item }) => <Text>{item.title}</Text>}
          keyExtractor={ (item, idx) => idx.toString() }
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
