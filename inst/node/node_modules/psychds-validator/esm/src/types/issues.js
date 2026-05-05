/**
 * Updated internal Issue structure for schema based validation
 */
export class Issue {
    key;
    severity;
    reason;
    requires;
    files;
    constructor({ key, severity, reason, requires, files, }) {
        this.key = key;
        this.severity = severity;
        this.reason = reason;
        this.requires = requires;
        // We want to be able to easily look up by path, so turn IssueFile[] into a Map
        if (Array.isArray(files)) {
            this.files = new Map();
            for (const f of files) {
                this.files.set(f.path, f);
            }
        }
        else {
            this.files = files;
        }
    }
    get helpUrl() {
        // Provide a link to NeuroStars
        return `https://neurostars.org/search?q=${this.key}`;
    }
}
