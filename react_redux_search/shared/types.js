// @flow

export type GemName = string

export type Gem =
  {| name : GemName
   , authors : Array<string>
   , info : string
   , version : string
   , downloads: number
   , saved : boolean
   |}

export type ResponseError =
  {| error : string
   |}

export type FetchGemsResponse = Array<Gem>

export type SaveGemResponse =
  {| name : string
   , status : string
   |}
export type RemoveGemResponse =
  {| name : string
   , status : string
   |}
export type FetchSavedGemsResponse = Array<Gem>   
