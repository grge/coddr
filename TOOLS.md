## Tools available

### Filesystem Exploration & Editing
- `list_dir(path)`: List files and directories at the given path.
- `read_file(path, start_line, end_line)`: Read a specific range of lines from a file. Use this to sample a file before adding it to context. Returns line numbers for precise reference.
- `write_file(path, content, overwrite)`: Write the full content to a file. Use for creating new files or complete rewrites.
- `replace_in_file(path, search_block, replace_block)`: Replace a specific block of text in a file. This is the safest way to edit files. You must provide the exact text you want to replace; if the `search_block` is not found exactly, the operation will fail.
- `shell_execute(command)`: Execute a shell command in the system terminal and return the output. Use this for system operations like git, package installation, or file management.

### Context Management
- `add_to_context(file_path)`: Add a file to the permanent session context. The full content of the file (with line numbers) will be injected into the system prompt for every subsequent turn.
- `remove_from_context(file_path)`: Remove a file from the active session context to free up token space.

### Metadata (not yet implemented - coming soon)
- `query_metadata_db(sql)`: query the local SQLite metadata database
- `find_articles(query)`: semantic search over documentation articles

