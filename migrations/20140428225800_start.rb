Sequel.migration do
  change do
    create_table(:schema_migrations) do
      String :filename, :text=>true, :null=>false

      primary_key [:filename]
    end

    create_table(:users, :ignore_index_errors=>true) do
      primary_key :id
      DateTime :created_at
      String :email
      String :first
      String :last
      String :password_hash
      String :password_salt

      index [:email], :name=>:users_email_idx, :unique=>true
    end

    create_table(:whiteboards, :ignore_index_errors=>true) do
      primary_key :id
      DateTime :created_at
      String :name
      Integer :user_id
      String :key, :default=>"", :null=>false
      TrueClass :active, :default=>true, :null=>false

      index [:key], :name=>:whiteboards_key, :unique=>true
      index [:user_id], :name=>:whiteboards_user_id_idx
      index [:user_id, :name], :name=>:whiteboards_user_id_name_idx, :unique=>true
    end
  end
end
