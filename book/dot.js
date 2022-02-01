/*
Language: Graphviz Dot
Author: Simon Johnston <johnstonskj@gmail.com>
Website: https://github.com/johnstonskj/rust-asdi
Category: graph
*/

hljsDot = function(hljs) {

  const GRAPH_KEYWORDS = [
    "strict",
    "graph",
    "digraph"
  ]

  const SUB_GRAPH_KEYWORDS = [
    "subgraph"
  ]

  const BOOLEAN_LITERALS = [
    "true",
    "false"
  ]

  const BUILT_IN_ATTRIBUTES = [
    "_background",
    "label",
    "shape",
    "width",
  ]

  const ATTR_LIST = {
    scope: 'punctuation',
    begin: /\[/,
    end: /\]/,
    keywords: {
      keyword: SUB_GRAPH_KEYWORDS,
      literal: BOOLEAN_LITERALS
    },
    contains: [
      hljs.HASH_COMMENT_MODE,
      hljs.C_BLOCK_COMMENT_MODE,
      hljs.C_LINE_COMMENT_MODE,

      hljs.QUOTE_STRING_MODE,
      hljs.NUMBER_MODE
    ]
  };

  const LINE_TERMINATOR = {
    scope: 'punctuation',
    begin: /;/
  };

  const EDGEOP = {
    scope: 'operator',
    begin: /\-/,
    end: /\-|>/
  };

  const BOOLEAN = {
    scope: 'literal',
    begin: /(true)|(false)/
  };

  const GRAPH_INNER = {
    scope: 'punctuation',
    begin: /{/,
    end: /}/,
    keywords: {
      keyword: SUB_GRAPH_KEYWORDS,
      literal: BOOLEAN_LITERALS
    },
    contains: [
      hljs.HASH_COMMENT_MODE,
      hljs.C_BLOCK_COMMENT_MODE,
      hljs.C_LINE_COMMENT_MODE,

      ATTR_LIST,

      hljs.QUOTE_STRING_MODE,
      hljs.NUMBER_MODE,
      hljs.TITLE_MODE,

      EDGEOP,

      LINE_TERMINATOR

      // TODO: SUB_GRAPH
    ]
  }

  const SUB_GRAPH = {
    begin: /subgraph/,
    endsWithParent: true,
    keywords: {
      keyword: SUB_GRAPH_KEYWORDS,
      literal: BOOLEAN_LITERALS
    },
    contains: [
      hljs.HASH_COMMENT_MODE,
      hljs.C_BLOCK_COMMENT_MODE,
      hljs.C_LINE_COMMENT_MODE,

      hljs.TITLE_MODE,

      GRAPH_INNER
    ]
  }

  const GRAPH = {
    begin: /strict|graph|digraph/,
    endsWithParent: true,
    keywords: {
      keyword: GRAPH_KEYWORDS,
      literal: BOOLEAN_LITERALS
    },
    contains: [
      hljs.HASH_COMMENT_MODE,
      hljs.C_BLOCK_COMMENT_MODE,
      hljs.C_LINE_COMMENT_MODE,

      hljs.TITLE_MODE,

      GRAPH_INNER
    ]
  }

  return {
    name: 'Dot',
    aliases: [ "graphviz" ],
    case_insensitive: true,
    keywords: GRAPH_KEYWORDS,
    built_in: BUILT_IN_ATTRIBUTES,
    contains: [
      hljs.HASH_COMMENT_MODE,
      hljs.C_BLOCK_COMMENT_MODE,
      hljs.C_LINE_COMMENT_MODE,

      GRAPH,
    ]
  };
}

// module.exports = hljsDot;
