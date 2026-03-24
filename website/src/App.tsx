import { Code } from "./components/code";
import { ExternalLink } from "./components/external-link";

export default function App() {
  return (
    <main className="flex flex-col gap-6 max-w-[600px] mx-auto p-5 pt-16 lg:px-10 lg:pt-16 leading-relaxed text-base">
      <div>
        <h1 className="text-lg font-semibold text-neutral-100">
          arc <span className="align-top text-sm leading-none">⌒</span>
        </h1>
        <p className="mt-1">JavaScript on the BEAM</p>
      </div>

      <picture>
        <source media="(prefers-color-scheme: dark)" srcSet="/js.png" />
        <img
          alt="JavaScript on the BEAM"
          src="/js-light.png"
          className="w-full rounded-lg"
        />
      </picture>

      <p>
        Traditionally, JavaScript does concurrency with one event loop and a
        shared heap. The BEAM does it with isolated processes that share nothing.
        Arc is an experiment in running the former on the latter.
      </p>

      <p>
        Arc is an entire JavaScript engine written in{" "}
        <ExternalLink href="https://gleam.run">Gleam</ExternalLink>. Every{" "}
        <Code>Arc.spawn</Code> is a real Erlang process. You can have millions
        of them, each with its own heap — no stop-the-world garbage collection,
        and a crash in one leaves the others untouched. These are guarantees
        JavaScript has never had.
      </p>

      <div>
        <p>
          Tested against{" "}
          <ExternalLink href="https://github.com/tc39/test262">
            test262
          </ExternalLink>{" "}
          on every commit:
        </p>
        <picture className="block mt-3">
          <source
            media="(prefers-color-scheme: dark)"
            srcSet="/conformance-dark.png"
          />
          <img
            alt="test262 conformance chart"
            src="/conformance.png"
            className="w-full rounded-lg"
          />
        </picture>
      </div>

      <hr className="w-12 border-neutral-800" />

      <div className="bg-neutral-900/50 rounded-lg p-4 font-mono text-sm text-neutral-300 space-y-1">
        <p>
          <span className="text-neutral-500 select-none">$ </span>gleam run --
          file.js
          <span className="text-neutral-600 ml-4"># run a script</span>
        </p>
        <p>
          <span className="text-neutral-500 select-none">$ </span>gleam test
          <span className="text-neutral-600 ml-4"># unit tests</span>
        </p>
        <p>
          <span className="text-neutral-500 select-none">$ </span>
          TEST262_EXEC=1 gleam test
          <span className="text-neutral-600 ml-4"># full test262 suite</span>
        </p>
      </div>

      <div className="flex items-center gap-4">
        <ExternalLink href="https://github.com/aidenybai/arc">
          GitHub
        </ExternalLink>
      </div>

      <p className="text-neutral-500 text-sm">
        Arc is an extremely early research project, tread carefully.
      </p>
    </main>
  );
}
