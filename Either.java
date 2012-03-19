
public abstract class Either<A, B> {
	public static enum Tag { LEFT, RIGHT }

	public final Tag tag;
	private Either(final Tag tag) {
		this.tag = tag;
	}

	public A left() { throw new UnsupportedOperationException(); }
	public B right() { throw new UnsupportedOperationException(); }

	public static final <A, B> Either<A, B> left(final A x) {
		return new Either<A, B>(Tag.LEFT) {
			@Override
			public final A left() {
				return x;
			}
		};
	}

	public static final <A, B> Either<A, B> right(final B x) {
		return new Either<A, B>(Tag.RIGHT) {
			@Override
			public final B right() {
				return x;
			}
		};
	}
}
