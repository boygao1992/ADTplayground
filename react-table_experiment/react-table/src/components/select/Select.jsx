import React from 'react'
import * as R from 'ramda'
import SelectItem from 'react-select'


/* const MODEL_ID = "id" */
const MODEL_NAME = "name"
const MODEL_YEAR = "year"
const MODEL_MAKE = "make"
const MODEL_TRIM = "trim"

const A = {
  // nub :: forall a. Array a -> Array a
  nub: arr => { return R.union(arr, [])}
}

/* Lens */

// foreign_getMakeName :: Array Car -> Maybe String
const foreign_getMakeName = R.compose(R.prop(MODEL_MAKE), R.head)

// foreign_getMakeList :: Map String (Array Car) -> Array { id :: String, name :: String }
function foreign_getMakeList (makeDict) {
  let result = []
  for (let makeId in makeDict) {
    result.push({value: makeId, label: foreign_getMakeName(R.prop(makeId , makeDict)) })
  }
  return result
}

// foreign_getModels :: String -> Array Car -> Array String
const foreign_getModels = makeId =>
  R.compose(
      A.nub
    , R.map(car => (R.prop(MODEL_NAME, car)) )
    , R.prop(makeId)
  )

// foreign_getYears :: String -> String -> Array String -> Array Int
const foreign_getYears = (makeId, modelName) =>
  R.compose(
      R.map(R.toString)
    , R.sort((x, y) => (x - y)) // NOTE small to large
    , A.nub
    , R.map( R.prop(MODEL_YEAR))
    , R.filter( R.propEq(MODEL_NAME, modelName))
    , R.prop(makeId)
  )

// foreign_getTrims :: String -> String -> String -> Array Car -> Array String
const foreign_getTrims = (makeId, modelName, year) =>
  R.compose (
      R.map ( R.prop(MODEL_TRIM) )
    , R.filter(
      R.allPass (
        [ R.propEq(MODEL_NAME, modelName)
        , R.compose (
          R.equals(year)
          , R.toString
          , R.prop(MODEL_YEAR)
        )
        ]
      )
    )
    , R.prop(makeId)
  )

const toOptions = R.map(value => ({value, label: value}))

/* Lenses */
const _foreignData = R.lensProp("foreign_data")

const _make = R.lensProp("make")
const _model = R.lensProp("model")
const _year = R.lensProp("year")
const _trim = R.lensProp("trim")

const _disabled = R.lensProp("disabled")
const _options = R.lensProp("options")
const _selection = R.lensProp("selection")

export default class Select extends React.Component {
  defaultState = {
    foreign_data: {},
    make: { disabled: true, options: [], selection: null },
    model: { disabled: true, options: [], selection: null },
    year: { disabled: true, options: [], selection: null },
    trim: { disabled: true, options: [], selection: null }
  }

  constructor(props) {
    super(props)
    this.state = this.defaultState
  }

  componentWillReceiveProps(nextProps) {
    if (nextProps.data !== this.state.foreign_data) {
      this.init(nextProps.data)
    }
  }

  init = data => {
    this.setState(state => {
        const make_options = foreign_getMakeList(data)

        return R.compose(
            R.set(_foreignData, data)
          , R.set(R.compose(_make, _options), make_options)
          , R.set(R.compose(_make, _disabled), false)
        )(state)
    })
  }

  selectMake = selectedOption => {
    if (selectedOption !== this.state.make.selection) {
      const makeId = selectedOption.value

      this.setState(state => {
        const model_options =
          R.compose(
            toOptions
            , foreign_getModels(makeId)
            , R.view(_foreignData)
          )(state)

        return R.compose(
            R.set(R.compose(_make, _selection), selectedOption)
          , R.set(R.compose(_model, _options), model_options)
          , R.set(R.compose(_model, _disabled), false)
          , R.set(R.compose(_model, _selection), null)
          , R.set(R.compose(_year, _options), [])
          , R.set(R.compose(_year, _disabled), true)
          , R.set(R.compose(_year, _selection), null)
          , R.set(R.compose(_trim, _options), [])
          , R.set(R.compose(_trim, _disabled), true)
          , R.set(R.compose(_trim, _selection), null)
        )(state)
      })
    }
  }

  selectModel = selectedOption => {
    if (selectedOption !== this.state.model.seletion) {
      const modelName = selectedOption.value

      this.setState(state => {
        const year_options =
          R.compose(
            toOptions
            , foreign_getYears(state.make.selection.value, modelName)
            , R.view(_foreignData)
          )(state)

        return R.compose(
            R.set(R.compose(_model, _selection), selectedOption)
          , R.set(R.compose(_year, _options), year_options)
          , R.set(R.compose(_year, _disabled), false)
          , R.set(R.compose(_trim, _options), [])
          , R.set(R.compose(_trim, _disabled), true)
        )(state)
      })
    }
  }

  selectYear = selectedOption => {
    if (selectedOption !== this.state.year.selection) {
      const year = selectedOption.value

      this.setState(state => {
        const trim_options =
          R.compose(
            toOptions
            , foreign_getTrims(state.make.selection.value, state.model.selection.value, year)
            , R.view(_foreignData)
          )(state)

        return R.compose(
            R.set(R.compose(_year, _selection), selectedOption)
          , R.set(R.compose(_trim, _options), trim_options)
          , R.set(R.compose(_trim, _disabled), false)
        )(state)
      })
    }
  }

  selectTrim = selectedOption => {
    if (selectedOption !== this.state.trim.selection) {
      this.setState(state => {
        return R.compose(
          R.set(R.compose(_trim, _selection), selectedOption)
        )(state)
      })
    }
  }

  render () {
    return (
      <div className="form__inputs">
        <ul>
          <li className="form__inputs__make">
            <h3>Vehicle Brand/Make*</h3>
            <SelectItem
              options={R.view(R.compose(_make, _options), this.state)}
              isDisabled={R.view(R.compose(_make, _disabled), this.state)}
              value={R.view(R.compose(_make, _selection), this.state)}
              onChange={this.selectMake}
            />
          </li>
          <li className="form__inputs__model">
            <h3>Vehicle Model*</h3>
            <SelectItem
              options={R.view(R.compose(_model, _options), this.state)}
              isDisabled={R.view(R.compose(_model, _disabled), this.state)}
              value={R.view(R.compose(_model, _selection), this.state)}
              onChange={this.selectModel}
            />
          </li>
          <li className="form__inputs__year">
            <h3>Vehicle Year*</h3>
            <SelectItem
              options={R.view(R.compose(_year, _options), this.state)}
              isDisabled={R.view(R.compose(_year, _disabled), this.state)}
              value={R.view(R.compose(_year, _selection), this.state)}
              onChange={this.selectYear}
            />
          </li>
          <li className="form__inputs__trim">
            <h3>Vehicle Trim*</h3>
            <SelectItem
              options={R.view(R.compose(_trim, _options), this.state)}
              isDisabled={R.view(R.compose(_trim, _disabled), this.state)}
              value={R.view(R.compose(_trim, _selection), this.state)}
              onChange={this.selectTrim}
            />
          </li>
        </ul>
      </div>
    )
  }
}
