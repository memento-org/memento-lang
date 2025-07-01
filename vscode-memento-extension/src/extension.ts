import * as vscode from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  Executable,
} from "vscode-languageclient/node";

let client: LanguageClient;

export function activate(context: vscode.ExtensionContext) {
  console.log("Memento extension activated");

  // Path to the LSP server executable - use stack exec to find it dynamically
  // Use the current workspace folder (should be the memento-compiler directory)
  const workspaceFolder = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
  if (!workspaceFolder) {
    vscode.window.showErrorMessage(
      "Memento LSP requires a workspace folder to be opened"
    );
    return;
  }
  const mementoCompilerPath = workspaceFolder;

  const serverExecutable: Executable = {
    command: "stack",
    args: ["exec", "memento-lsp-exe"],
    options: {
      cwd: mementoCompilerPath,
    },
  };

  const serverOptions: ServerOptions = serverExecutable;

  const clientOptions: LanguageClientOptions = {
    documentSelector: [
      { scheme: "file", language: "memento" },
      { scheme: "file", pattern: "**/*.mmt" },
      { pattern: "**/*.mmt" },
    ],
    outputChannelName: "Memento Language Server",
  };

  client = new LanguageClient(
    "mementoLanguageServer",
    "Memento Language Server",
    serverOptions,
    clientOptions
  );

  // Start the client
  client.start();

  // Register test command
  const testCommand = vscode.commands.registerCommand("memento.test", () => {
    vscode.window.showInformationMessage(
      "Memento LSP Test - check output channel"
    );
    console.log("Test command executed");
  });

  // Register restart LSP command
  const restartCommand = vscode.commands.registerCommand("memento.restartLSP", async () => {
    vscode.window.showInformationMessage("Restarting Memento Language Server...");
    
    if (client) {
      await client.stop();
      await client.start();
      vscode.window.showInformationMessage("Memento Language Server restarted successfully");
    }
  });

  context.subscriptions.push(testCommand);
  context.subscriptions.push(restartCommand);

  console.log("Memento LSP client started");
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
