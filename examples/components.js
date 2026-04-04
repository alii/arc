// Minimal React-like components on Arc.
// Each component runs in its own process with local state.

// --- Framework ---

let _hooks = [];
let _hookIdx = 0;
let _dirty = false;

const createElement = (type, props, children) => ({
  type: type || "Process",
  props: props || {},
  children: children || [],
});

const useState = (initial) => {
  const i = _hookIdx++;
  if (i >= _hooks.length) _hooks.push(initial);
  const val = _hooks[i];
  const set = (next) => {
    _hooks[i] = next;
    _dirty = true;
  };
  return [val, set];
};

const mount = (element) => {
  if (typeof element === "string" || typeof element === "number") {
    return `${element}`;
  }
  if (typeof element.type === "string") {
    return {
      tag: element.type,
      props: element.props,
      children: element.children.map(mount),
    };
  }

  // Function component → spawn as its own process
  const caller = Arc.self();
  const { type: fn, props, children } = element;
  props.children = children;

  Arc.spawn(() => {
    let output;
    _dirty = true;
    while (_dirty) {
      _hookIdx = 0;
      _dirty = false;
      output = fn(props);
    }
    Arc.send(caller, mount(output));
  });

  return Arc.receive(3000);
};

const stringify = (node) => {
  if (typeof node === "string") return node;
  const kids = node.children.map(stringify).join("");
  if (kids) return `<${node.tag}>${kids}</${node.tag}>`;
  return `<${node.tag} />`;
};

const render = (element) => {
  Arc.log(stringify(mount(element)));
};

// --- Components ---

const Counter = (props) => {
  const [count, setCount] = useState(0);
  if (count < 3) setCount(count + 1);
  return createElement(null, { label: props.label }, [count]);
};

const Greeting = (props) => {
  return createElement(null, {}, [`Hello, ${props.name}!`]);
};

const App = () => {
  return createElement(null, {}, [
    createElement(Greeting, { name: "Arc" }),
    createElement(Counter, { label: "A" }),
    createElement(Counter, { label: "B" }),
  ]);
};

// --- Entry point ---

render(createElement(App));
