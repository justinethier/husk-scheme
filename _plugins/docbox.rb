module Jekyll
  class DocBlock < Liquid::Block
    include Liquid::StandardFilters

    def initialize(tag_name, markup, tokens)
      super
      #TODO: error handling (see highlight example)
      #params = markup.split(" ")
      params = markup.scan(/"([^"]*)" (.*)/)[0]
      @call = params[0]
      @type = params[1]
    end

    def render(context)
      code = super
      <<-HTML
#{@call} <br />
#{@type} <br />
      markup:
      #{@code}
      HTML
    end
  end
end

Liquid::Template.register_tag('docbox', Jekyll::DocBlock)
