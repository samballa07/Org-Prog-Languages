require_relative "graph.rb"

class Synsets
    @synsets
    def initialize
      @synsets = Hash.new
    end

    def load(synsets_file)
      
      file = File.open(synsets_file)
      invalid = Array.new
      line_number = 1
      hash = Hash.new
      data = file.readlines.map(&:chomp)

      data.each do |line|
        if !line.match(/^id: (\d+) synset: (([\w\-.'\/]+)(,[\w\-.'\/]+)*)$/) then
          invalid.push line_number

        elsif @synsets.has_key?($1.to_i) || $1.to_i < 0 || 
              line.empty? ||hash.has_key?($1.to_i) then
          invalid.push line_number

        else 
          nouns = $2.split(',')
          hash[$1.to_i] = nouns
          
        end
        line_number += 1
      end

      file.close
      if invalid.empty? then
        hash.each do |id, nouns|
          self.addSet(id, nouns)
        end
        return nil
      end
      
      return invalid
    end

    def addSet(synset_id, nouns)
      if synset_id < 0 || nouns.empty? || @synsets.has_key?(synset_id) then
        return false
      end
      nouns.each do |word|
        if !word.match(/^[\w\-.'\/]+$/) then
          return false
        end
      end

      @synsets[synset_id] = nouns
      return true  
    end

    def lookup(synset_id)
      
      if @synsets.has_key?(synset_id) then
        return @synsets[synset_id]
      else
        return Array.new
      end
    end

    def findSynsets(to_find)
      if(to_find.is_a? String) then
        arr_ids = Array.new

        @synsets.each do |id, nouns|
          if nouns.include?(to_find) then
            arr_ids.push(id)
          end
        end
        return arr_ids

      elsif(to_find.is_a? Array) then
        hash_ids = Hash.new

        for i in 0...to_find.length do
          arr_ids = Array.new
          
          @synsets.each do |id, nouns|
            if nouns.include?(to_find[i]) then
              arr_ids.push(id)
            end
          end
          hash_ids[to_find[i]] = arr_ids
        end
        return hash_ids
      end

      return nil
    end

    def getSynsetIds 
      return @synsets.keys
    end
end

class Hypernyms
    @hypernyms

    def initialize
      @hypernyms = Graph.new
    end

    def load(hypernyms_file)
      file = File.open(hypernyms_file)
      data = file.readlines.map(&:chomp)
      invalid = Array.new 
      hash = Hash.new
      line_number = 1

      data.each do |line|
        hypernym_ids = Array.new
        synset_id = Integer

        if line.match(/^from: (\d+) to: (\d+)(,\d+)*$/) then
          synset_id = $1.to_i
          hypernym_ids = $2.split(',')
          hypernym_ids.map!(&:to_i)
        end

        if !line.match(/^from: (\d+) to: (\d+)(,\d+)*$/) 
          invalid.push line_number
        
        elsif synset_id.to_i < 0 ||hypernym_ids.any? {|id| id < 0}
          invalid.push line_number

        else
          hash[synset_id] = hypernym_ids
        end

        line_number += 1
      end
      file.close

      if invalid.empty? then
        hash.each do |from_id, to_ids|
          to_ids.each do |id|
            self.addHypernym from_id, id
          end
        end
        return nil
      end
      
      return invalid
    end

    def addHypernym(source, destination)
      if source == destination || source < 0 || destination < 0 then
        return false
      end

      if !@hypernyms.hasVertex? source then
        @hypernyms.addVertex source
      end
      if !@hypernyms.hasVertex? destination then
        @hypernyms.addVertex destination
      end
      if !@hypernyms.hasEdge? source, destination then
        @hypernyms.addEdge(source, destination)
      end
      return true
    end

    def lca(id1, id2)
      if @hypernyms.hasVertex?(id1) && @hypernyms.hasVertex?(id2)
        vertices1 = @hypernyms.bfs(id1)
        vertices2 = @hypernyms.bfs(id2)
        lca = Array.new
        ancestors = Hash.new

        vertices1.each do |vertex, height|
          if vertices2.has_key? vertex then
            height += vertices2[vertex]
            ancestors[vertex] = height
          end
        end

        min = ancestors.values.min()
        ancestors.each do |vertex, height|
          if height == min then
            lca.push vertex
          end
        end
      
        return lca
      else
        return nil
      end
    end

    def getVertices
      return @hypernyms.vertices
    end

end

class CommandParser
    def initialize
        @synsets = Synsets.new
        @hypernyms = Hypernyms.new
        @result = Hash.new
    end

    def parse(command)
      command_arr = command.split(" ")
      @result = {}

      if command_arr[0] == "load" 
        @result[:result] = load(command)
        
      elsif command_arr[0] == "lookup"
        @result[:result] = lookup(command)

      elsif command_arr[0] == "find"
        @result[:result] = find(command)

      elsif command_arr[0] == "findmany"
        @result[:result] = findMany(command)

      elsif command_arr[0] == "lca"
        @result[:result] = lca(command)

      else
        @result[:recognized_command] = :invalid  
      end
      return @result
    end

    def load(command)
      @result[:recognized_command] = :load
      commands = command.split(" ")
      
      if commands.length != 3 || !commands[1].match(/^([\w\-.\/]+)$/) ||
        !commands[2].match(/^([\w\-.\/]+)$/) then
        return :error
      end
    
      temp_synsets = Synsets.new
	    temp_hypernyms = Hypernyms.new
      result_synset = temp_synsets.load commands[1]
      result_hypernyms = temp_hypernyms.load commands[2]

      arr_vert = temp_hypernyms.getVertices
      loaded_keys = temp_synsets.getSynsetIds
      curr_object_keys = @synsets.getSynsetIds

      arr_vert.each do |id|
         if !loaded_keys.include?(id) && 
            !curr_object_keys.include?(id) then
            return false
         end
      end

      if result_hypernyms == nil && result_synset == nil 
        @synsets.load commands[1]
        @hypernyms.load commands[2]
        return true          
      end

      return false
    end

    def lookup(command)
      @result[:recognized_command] = :lookup
      commands = command.split(" ")
      if !command.match(/^\s*lookup\s+(\d+)\s*$/) then
        return :error
      end
      id = $1.to_i

      if commands.length != 2 || id < 0 then
        return :error

      else
        return @synsets.lookup id         
      end
    end

    def find(command)
      @result[:recognized_command] = :find
      commands = command.split(" ")
      if !command.match(/^\s*find\s+[\w\-.'\/]+\s*$/) then
        return :error
      end
      return @synsets.findSynsets(commands[1])
    end
    
    def findMany(command)
      @result[:recognized_command] = :findmany
      commands = command.split(" ")

      if commands.length == 2 then
        nouns = commands[1].split(',')

        nouns.each do |word| 
          if !word.match(/^[\w\-.'\/]+$/) then
            return :error
          end
        end

        return @synsets.findSynsets(nouns)
      end

      return :error
    end

    def lca(command)
      @result[:recognized_command] = :lca
      commands = command.split(" ")
      
      if !command.match(/^\s*lca\s+(\d+)\s+(\d+)\s*$/) then
        return :error
      end
      id1 = $1.to_i
      id2 = $2.to_i
      if commands.length != 3 || id1 < 0 || id2 < 0 then
        return :error
      end
      return @hypernyms.lca(id1, id2)
    end
end
