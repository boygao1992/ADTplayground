json.extract! purchase, :id, :name, :cost, :created_at, :updated_at
json.url purchase_url(purchase, format: :json)
