use criterion::{criterion_group, criterion_main, Criterion};
use std::time::Duration;

pub mod common;
pub use common::*;

// ------------------------------------------------------------------------------------------------
// Benchmark Wrappers
// ------------------------------------------------------------------------------------------------

fn benchmark_group(c: &mut Criterion) {
    let mut group = c.benchmark_group("parser");
    group
        .significance_level(0.1)
        .sample_size(500)
        .measurement_time(Duration::from_secs(10));
    group.bench_function("socrates", |b| b.iter(socrates_program));
    group.bench_function("paths", |b| b.iter(paths_program));
    group.bench_function("ancestors", |b| b.iter(ancestors_program));
    group.bench_function("rdf_schema", |b| b.iter(rdf_schema_program));
    group.finish();
}

criterion_group!(benches, benchmark_group);
criterion_main!(benches);
