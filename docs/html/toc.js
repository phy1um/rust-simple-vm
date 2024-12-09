(function(){
  function makeNode(node, children) {
    return {node, children};
  }

  function treeWalkNodeName(n, tagName) {
    const children = [];
    const visit = [n];
    for (let i = 0; i < visit.length; i++) {
      const tgt = visit[i];
      for (let c of tgt.children) {
        if (c.tagName === tagName) {
          children.push(treeWalkNodeName(c, tagName));
        } else {
          visit.push(c);
        }
      }
    }
    return makeNode(n, children);
  }

  function getTitle(node) {
    for (let i = 1; i < 7; i++) {
      const tagName = `h${i}`;
      for (let x of node.getElementsByTagName(tagName)) {
        return x.innerText; 
      }
    }
    return "NO-TITLE";
  }

  function populateTOC(target, opts) {
    opts = opts || {};
    const prefix = opts.prefix || "section";
    const defaultConvertTitle = (s) => prefix + "-" + s.toLowerCase().replaceAll(" ", "-");
    const convertTitle = opts.convertTitle || defaultConvertTitle;
    const tree = treeWalkNodeName(document.body, "SECTION");

    function appendTOCLayer(target, children) {
      if (children.length == 0) {
        return;
      }
      const ul = document.createElement("ul");
      target.appendChild(ul);
      for (let c of children) {
        const title = getTitle(c.node);
        const idName = convertTitle(title);
        c.node.id = idName;
        const li = document.createElement("li");
        ul.appendChild(li);
        const a = document.createElement("a");
        a.href = `#${idName}`;
        a.innerText = title;
        li.appendChild(a);
        appendTOCLayer(li, c.children);
      }
    }

    appendTOCLayer(target, tree.children);
  }

  window.populateTOC = populateTOC;
})();
