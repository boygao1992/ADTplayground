require 'sinatra'
require 'json'
require 'sinatra/json'

configure { set :server, :puma }

get "/" do
  "hello world"
end

get "/json" do
  json ( { value: "good"} )
end

post "/json" do
  payload = JSON.parse(request.body.read, symbolize_names: true)
  logger.info "id: #{payload[:id]}"
end
