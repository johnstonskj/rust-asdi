use asdi::idb::eval::NaiveEvaluator;
use criterion::{criterion_group, criterion_main, Criterion};
use std::time::Duration;

pub mod common;
pub use common::*;

// ------------------------------------------------------------------------------------------------
// Benchmark Wrappers
// ------------------------------------------------------------------------------------------------

fn benchmark_group(c: &mut Criterion) {
    let mut group = c.benchmark_group("evaluator-naive");
    group
        .significance_level(0.1)
        .sample_size(500)
        .measurement_time(Duration::from_secs(10));
    group.bench_with_input("socrates", &socrates_program(), |b, p| {
        b.iter(|| eval_socrates(p, NaiveEvaluator::default()))
    });
    group.bench_with_input("ancestors", &ancestors_program(), |b, p| {
        b.iter(|| eval_ancestors(p, NaiveEvaluator::default()))
    });
    group.bench_with_input("paths", &paths_program(), |b, p| {
        b.iter(|| eval_paths(p, NaiveEvaluator::default()))
    });
    group.bench_with_input("rdf_schema", &rdf_schema_program(), |b, p| {
        b.iter(|| eval_rdf_schema(p, NaiveEvaluator::default()))
    });
    group.finish();
}

criterion_group!(benches, benchmark_group);
criterion_main!(benches);
