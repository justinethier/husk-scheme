# Module to conveniently output Scheme API documentation
module Jekyll
  class DocBlock < Liquid::Block
    include Liquid::StandardFilters

    def initialize(tag_name, markup, tokens)
      super
      #TODO: error handling (see highlight example)
      #params = markup.split(" ")
      params = markup.scan(/([^ ]*) "([^"]*)" (.*)/)[0]
      @name = params[0]
      @call = params[1]
      @type = params[2]
    end

    def render(context)
      code = super
      <<-HTML
    <div id="#{@name}" class="member"> 
        <div>
          <label>#{@type}</label>
          <strong>#{@call}</strong>
        </div>
      #{code}
    </div>
      HTML
    end
  end
end

Liquid::Template.register_tag('scmdoc', Jekyll::DocBlock)
