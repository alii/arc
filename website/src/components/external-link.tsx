const link =
	'underline decoration-rpd-muted/70 underline-offset-[3px] decoration-1 hover:decoration-rpd-subtle dark:decoration-rp-muted/70 dark:hover:decoration-rp-subtle';

export function ExternalLink({ href, children }: { href: string; children: React.ReactNode }) {
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
