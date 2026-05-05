export class FileTree {
    // Relative path to this FileTree location
    path;
    // Name of this directory level
    name;
    files;
    directories;
    parent;
    constructor(path, name, parent) {
        this.path = path;
        this.files = [];
        this.directories = [];
        this.name = name;
        this.parent = parent;
    }
    contains(parts) {
        if (parts.length === 0) {
            return false;
        }
        else if (parts.length === 1) {
            return this.files.some((x) => x.name === parts[0]);
        }
        else if (parts.length > 1) {
            const nextDir = this.directories.find((x) => x.name === parts[0]);
            if (nextDir) {
                return nextDir.contains(parts.slice(1, parts.length));
            }
            else {
                return false;
            }
        }
        else {
            return false;
        }
    }
}
