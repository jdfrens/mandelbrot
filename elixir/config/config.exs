use Mix.Config

config :logger, backends: [:console], compile_time_purge_level: :info

config :fractals, unimplemented: [:newton, :nova]

config :porcelain, goon_warn_if_missing: false
