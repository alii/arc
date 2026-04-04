export function Code({ children }: { children: React.ReactNode }) {
	return (
		<code className="text-sm bg-rpd-overlay text-rpd-text dark:bg-rp-overlay dark:text-rp-text px-1 py-0.5 rounded">
			{children}
		</code>
	);
}
