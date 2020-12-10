require "minitest/autorun"
require_relative "../../src/wordnet.rb"

$VALID_SYNSETS = "inputs/public_synsets_valid"
$INVALID_SYNSETS = "inputs/public_synsets_invalid"
$VALID_HYPERNYMS = "inputs/public_hypernyms_valid"
$INVALID_HYPERNYMS = "inputs/public_hypernyms_invalid"

class PublicTests < MiniTest::Test
    def setup
        @synsets = Synsets.new
        @hypernyms = Hypernyms.new
    end

    def test_public_commandline
      parser = CommandParser.new
      assert_equal({:recognized_command => :load, :result => true},
                   parser.parse("load #{$VALID_SYNSETS} #{$VALID_HYPERNYMS}"))
      
  end

end
