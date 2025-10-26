// Populate the sidebar
//
// This is a script, and not included directly in the page, to control the total size of the book.
// The TOC contains an entry for each page, so if each page includes a copy of the TOC,
// the total size of the page becomes O(n**2).
class MDBookSidebarScrollbox extends HTMLElement {
    constructor() {
        super();
    }
    connectedCallback() {
        this.innerHTML = '<ol class="chapter"><li class="chapter-item expanded "><a href="introduction/introduction.html"><strong aria-hidden="true">1.</strong> Introduction</a></li><li class="chapter-item expanded "><a href="datalog/index.html"><strong aria-hidden="true">2.</strong> Datalog</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="datalog/abstract.html"><strong aria-hidden="true">2.1.</strong> Abstract Syntax</a></li><li class="chapter-item expanded "><a href="datalog/concrete.html"><strong aria-hidden="true">2.2.</strong> Concrete Syntax</a></li><li class="chapter-item expanded "><a href="datalog/features.html"><strong aria-hidden="true">2.3.</strong> Language Features</a></li></ol></li><li class="chapter-item expanded "><a href="model/index.html"><strong aria-hidden="true">3.</strong> Model API</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="model/programs.html"><strong aria-hidden="true">3.1.</strong> Programs</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="model/program_features.html"><strong aria-hidden="true">3.1.1.</strong> Features</a></li><li class="chapter-item expanded "><a href="model/program_parser.html"><strong aria-hidden="true">3.1.2.</strong> Parser</a></li><li class="chapter-item expanded "><a href="model/program_visitor.html"><strong aria-hidden="true">3.1.3.</strong> Visitor</a></li></ol></li><li class="chapter-item expanded "><a href="model/relations.html"><strong aria-hidden="true">3.2.</strong> Relations</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="model/relation_io.html"><strong aria-hidden="true">3.2.1.</strong> Input/Output</a></li></ol></li><li class="chapter-item expanded "><a href="model/rules.html"><strong aria-hidden="true">3.3.</strong> Rules</a></li><li class="chapter-item expanded "><a href="model/eval.html"><strong aria-hidden="true">3.4.</strong> Evaluation</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="model/eval_strata.html"><strong aria-hidden="true">3.4.1.</strong> Stratification</a></li></ol></li><li class="chapter-item expanded "><a href="model/queries.html"><strong aria-hidden="true">3.5.</strong> Queries &amp; Views</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="model/query_relational.html"><strong aria-hidden="true">3.5.1.</strong> Relational Algebra</a></li></ol></li><li class="chapter-item expanded "><a href="model/errors.html"><strong aria-hidden="true">3.6.</strong> Error Handling</a></li></ol></li><li class="chapter-item expanded "><a href="extension/index.html"><strong aria-hidden="true">4.</strong> Model Extension</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="extension/i_o.html"><strong aria-hidden="true">4.1.</strong> RelationSet I/O</a></li><li class="chapter-item expanded "><a href="extension/evaluator.html"><strong aria-hidden="true">4.2.</strong> Writing an Evaluator</a></li><li class="chapter-item expanded "><a href="extension/formatting.html"><strong aria-hidden="true">4.3.</strong> Writing a Formatter</a></li><li class="chapter-item expanded "><a href="extension/contributing.html"><strong aria-hidden="true">4.4.</strong> Contributing</a></li></ol></li><li class="chapter-item expanded "><a href="reference/abstract_graph.html"><strong aria-hidden="true">5.</strong> Appendix: Abstract Graphical View</a></li><li class="chapter-item expanded "><a href="reference/datalog_ebnf.html"><strong aria-hidden="true">6.</strong> Appendix: Datalog EBNF</a></li><li class="chapter-item expanded "><a href="reference/relational.html"><strong aria-hidden="true">7.</strong> Appendix: Relational Algebra Mapping</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="reference/relational_ebnf.html"><strong aria-hidden="true">7.1.</strong> Relational Algebra EBNF</a></li></ol></li><li class="chapter-item expanded "><a href="reference/logic.html"><strong aria-hidden="true">8.</strong> Appendix: Horn Clause Mapping</a></li><li class="chapter-item expanded "><a href="reference/references.html"><strong aria-hidden="true">9.</strong> Appendix: References</a></li></ol>';
        // Set the current, active page, and reveal it if it's hidden
        let current_page = document.location.href.toString().split("#")[0].split("?")[0];
        if (current_page.endsWith("/")) {
            current_page += "index.html";
        }
        var links = Array.prototype.slice.call(this.querySelectorAll("a"));
        var l = links.length;
        for (var i = 0; i < l; ++i) {
            var link = links[i];
            var href = link.getAttribute("href");
            if (href && !href.startsWith("#") && !/^(?:[a-z+]+:)?\/\//.test(href)) {
                link.href = path_to_root + href;
            }
            // The "index" page is supposed to alias the first chapter in the book.
            if (link.href === current_page || (i === 0 && path_to_root === "" && current_page.endsWith("/index.html"))) {
                link.classList.add("active");
                var parent = link.parentElement;
                if (parent && parent.classList.contains("chapter-item")) {
                    parent.classList.add("expanded");
                }
                while (parent) {
                    if (parent.tagName === "LI" && parent.previousElementSibling) {
                        if (parent.previousElementSibling.classList.contains("chapter-item")) {
                            parent.previousElementSibling.classList.add("expanded");
                        }
                    }
                    parent = parent.parentElement;
                }
            }
        }
        // Track and set sidebar scroll position
        this.addEventListener('click', function(e) {
            if (e.target.tagName === 'A') {
                sessionStorage.setItem('sidebar-scroll', this.scrollTop);
            }
        }, { passive: true });
        var sidebarScrollTop = sessionStorage.getItem('sidebar-scroll');
        sessionStorage.removeItem('sidebar-scroll');
        if (sidebarScrollTop) {
            // preserve sidebar scroll position when navigating via links within sidebar
            this.scrollTop = sidebarScrollTop;
        } else {
            // scroll sidebar to current active section when navigating via "next/previous chapter" buttons
            var activeSection = document.querySelector('#sidebar .active');
            if (activeSection) {
                activeSection.scrollIntoView({ block: 'center' });
            }
        }
        // Toggle buttons
        var sidebarAnchorToggles = document.querySelectorAll('#sidebar a.toggle');
        function toggleSection(ev) {
            ev.currentTarget.parentElement.classList.toggle('expanded');
        }
        Array.from(sidebarAnchorToggles).forEach(function (el) {
            el.addEventListener('click', toggleSection);
        });
    }
}
window.customElements.define("mdbook-sidebar-scrollbox", MDBookSidebarScrollbox);
