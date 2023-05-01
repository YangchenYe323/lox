/** @type {import('next').NextConfig} */
const nextConfig = {
  basePath: "/lox",
  images: {
    unoptimized: true,
  },
  reactStrictMode: true,
  output: "export",
  webpack: function (config, options) {
    config.experiments = {
      asyncWebAssembly: true,
    };
    config.output.webassemblyModuleFilename = "static/wasm/[modulehash].wasm";

    return config;
  },
};

module.exports = nextConfig;
