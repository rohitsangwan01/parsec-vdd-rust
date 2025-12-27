<img src="https://i.imgur.com/dDUa6GH.png" width="0" height="0" />

<p align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="https://github.com/user-attachments/assets/74e7db71-6166-49ae-b6c5-7543b15c60eb">
    <img alt="Parsec Virtual Display Driver" src="https://github.com/user-attachments/assets/57202381-021c-428a-ae38-0fc4b2e0ee0c">
  </picture>
</p>

# Parsec VDD Rust

[![Rust](https://img.shields.io/badge/rust-1.0+-orange.svg)](https://www.rust-lang.org/)
[![License](https://img.shields.io/badge/license-BSD-blue.svg)](LICENSE)

Unofficial Rust client library and CLI tool for the [Parsec Virtual Display Driver](https://github.com/nomi-san/parsec-vdd). This project provides a safe, idiomatic Rust interface for managing virtual displays on Windows using the Parsec VDD driver.

## Features

- 🖥️ **Virtual Display Management**: Add, remove, and configure virtual displays programmatically
- 📚 **Library API**: Use as a library in your Rust projects
- 🖱️ **CLI Tool**: Interactive command-line interface for managing displays
- 🔧 **Display Configuration**: Configure resolution, refresh rate, position, and orientation
- ✅ **Device Status Checking**: Query and verify driver status
- 🚀 **Thread-Safe**: Safe concurrent access to device handles

## Prerequisites

- **Windows 10/11** (required for Parsec VDD driver)
- **Rust 1.70+** (for building from source)
- **Parsec Virtual Display Driver** installed and running

### Installing the Parsec VDD Driver

1. Download the driver from the [official repository](https://github.com/nomi-san/parsec-vdd)
2. Follow the installation instructions in the driver's README
3. Ensure the driver is properly installed and the device status is `Ok`

## Installation

### From Source

```bash
# Clone the repository
git clone https://github.com/yourusername/parsec-vdd-rust.git
cd parsec-vdd-rust

# Build the project
cargo build --release

# The binary will be in target/release/parsec-vdd-rust.exe
```

### As a Library Dependency

Add this to your `Cargo.toml`:

```toml
[dependencies]
parsec-vdd-rust = { path = "../parsec-vdd-rust" }
# Or from git:
# parsec-vdd-rust = { git = "https://github.com/yourusername/parsec-vdd-rust.git" }
```

## Usage

### CLI Application

Run the CLI tool:

```bash
parsec-vdd-rust.exe --help
```

The CLI provides an interactive interface with the following commands:

## Credits

- **Parsec** - For the original Virtual Display Driver
- **nomi-san** - For the [Parsec VDD project](https://github.com/nomi-san/parsec-vdd)