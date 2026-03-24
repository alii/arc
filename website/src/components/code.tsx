export function Code({ children }: { children: React.ReactNode }) {
  return (
    <code className="text-sm bg-neutral-800 text-neutral-200 px-1 py-0.5 rounded">
      {children}
    </code>
  );
}
