MIGRATIONS_PATH = File.join( File.dirname(__FILE__), 'migrations' )

namespace :db do
  desc "Run migrations"
  task :migrate, [:version] do |t, args|
    require "sequel"
    Sequel.extension :migration
    db = Sequel.connect(ENV.fetch("DATABASE_URL"))
    if args[:version]
      puts "Migrating to version #{args[:version]}"
      Sequel::Migrator.run(db, MIGRATIONS_PATH, target: args[:version].to_i)
    else
      puts "Migrating to latest"
      Sequel::Migrator.run(db, MIGRATIONS_PATH)
    end
  end

  desc "create new migration file"
  task :gen_migration, [:name] do |t, args|
    name = args[:name].gsub(/\s/, '_').downcase
    filename = "#{ Time.now.strftime('%Y%m%d%H%M%S') }_#{ name }.rb"
    migration_file = File.join( MIGRATIONS_PATH, filename )

    puts "creating migration: #{ filename }"

    File.open( migration_file, 'w' ) do |f|
      f.write <<-EOF
# #{ name }
Sequel.migration do
  up do
    
  end

  down do
    
  end
end
      EOF
    end
  end
end

