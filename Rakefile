require 'json'
require 'yaml'

task :json do
  FileUtils.rm Dir.glob('json/*.json')

  Dir['yaml/*.yml'].each do |yaml_filename|
    json_filename = File.join('json', File.basename(yaml_filename, ".yml")) + ".json"
    yml = File.read(yaml_filename)
    data = YAML::load(yml)
    json = JSON.dump(data)
    File.write(json_filename, json)
  end
end
