import { Code } from "./components/code";

const link =
  "underline decoration-neutral-600/70 underline-offset-[3px] decoration-1 hover:decoration-neutral-400";

function ExternalLink({
  href,
  children,
}: {
  href: string;
  children: React.ReactNode;
}) {
  return (
    <a href={href} className={link} target="_blank" rel="noopener noreferrer">
      {children}
      <svg
        xmlns="http://www.w3.org/2000/svg"
        width="24"
        height="24"
        viewBox="0 0 24 24"
        fill="none"
        stroke="currentColor"
        strokeWidth="2"
        strokeLinecap="round"
        strokeLinejoin="round"
        className="w-3 h-3 inline-block ml-1 align-baseline"
      >
        <path d="M9 6.65032C9 6.65032 15.9383 6.10759 16.9154 7.08463C17.8924 8.06167 17.3496 15 17.3496 15M16.5 7.5L6.5 17.5" />
      </svg>
    </a>
  );
}

export default function App() {
  return (
    <main className="flex flex-col gap-2 max-w-[600px] mx-auto p-5 pt-16 lg:px-10 lg:pt-16 leading-relaxed text-base">
      <div>
        <h1 className="text-lg font-semibold text-neutral-100">arc <span className="align-top text-sm leading-none">⌒</span></h1>
      </div>
      <div>
        <p>
          JavaScript on the{" "}
          <ExternalLink href="https://www.erlang.org/doc/system/getting_started.html">
            BEAM
          </ExternalLink>
          .
        </p>
      </div>
      <div>
        <p>
          Traditionally, JavaScript does concurrency with one event loop and a
          shared heap. The BEAM does it with isolated processes that share
          nothing. Arc is an experiment in running the former on the latter.
        </p>
      </div>
      <div>
        <p>
          Arc is an entire JavaScript engine written in{" "}
          <ExternalLink href="https://gleam.run">Gleam</ExternalLink>. Every{" "}
          <Code>Arc.spawn</Code>{" "}
          is a real Erlang process. You can have millions of them, each with its
          own heap — no stop-the-world garbage collection, and a crash in one
          leaves the others untouched. These are guarantees JavaScript has never
          had.
        </p>
      </div>
      <div>
        <p>
          Tested against{" "}
          <ExternalLink href="https://github.com/tc39/test262">
            test262
          </ExternalLink>{" "}
          on every commit.
        </p>
      </div>
      <div>
        <hr className="w-12 border-neutral-800 my-2" />
      </div>
      <div>
        <p className="text-neutral-500 text-sm">
          Arc is an extremely early research project, tread carefully.
        </p>
      </div>
      <div>
        <p>
          <ExternalLink href="https://github.com/aidenybai/arc">
            GitHub
          </ExternalLink>
        </p>
      </div>
    </main>
  );
}
