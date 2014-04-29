# add_invites
Sequel.migration do
  up do
    create_table(:whiteboard_invites) do
      primary_key :id
      DateTime :created_at
      String :key, :default => '', :null => false
      Integer :user_id, :null => false
      Integer :board_id, :null => false

      index [:key], :name => :whiteboard_invites_key_idx, :unique => true
      index [:user_id], :name => :whiteboard_invites_user_id_idx
      index [:board_id], :name => :whiteboard_invites_board_id_idx
    end

    # I know there's a better way to do this.
    self[:whiteboards].all.each do |board|
      self[:whiteboard_invites].insert(
        :key => board[:key],
        :user_id => board[:user_id],
        :board_id => board[:id],
        :created_at => Sequel.function(:NOW)
      )
    end

    alter_table(:whiteboards) do
      drop_column :key
    end
  end
end
