package com.xml;

import java.util.Collections;
import java.util.Iterator;

/**
 *
 * Properties (could make them unit tests) = {
 *	forall T. forall x:Maybe<T>. x.isNull() -> (x.value() = throw NullPointerException())
 * 	forall T. forall x:Maybe<T>. !x.isNull() -> (exists y:T.  x.value() = y && y != null)
 *
 * 	forall T. <T>nothing().isNull()
 * 	forall T. forall x:T. (x == null) -> <T>just(x) = throw NullPointerException()
 * 	forall T. forall x:T. (x != null) -> !<T>just(x).isNull()
 *
 *  forall T. forall x:T. (x == null) -> <T>from(x) = <T>nothing()
 *  forall T. forall x:T. (x != null) -> <T>from(x) = <T>just(x)
 * }
 *
 * Examples:
 *
 * 	Maybe<C> g(B input) { .. }
 *  Maybe<B> f(A input) { .. }
 *
 *  Maybe<C> validate(Maybe<A> x) {
 * 		if(x.isNull()) return <C>nothing();
 *
 *  	Maybe<B> y = f(x.value());
 *  	if(y.isNull()) return <C>nothing();
 *
 *  	Maybe<C> z = g(y.value());
 *  	if(z.isNull()) return <C>nothing();
 *
 *  	return <C>just(z.value());
 * }
 *
 */
public abstract class Maybe<T> implements Iterable<T> {

	private Maybe() { }

	public final boolean getIsNull() {
		return isNull();
	}

	public final T getValue() {
		return value();
	}

	public abstract boolean isNull();
	public abstract T value();
	public abstract T or(final T defaultValue);
	public abstract Maybe<T> or(final Maybe<T> other);
	public abstract <E extends Exception> T except(final E ex) throws E;
	public final T except() throws MaybeNullException {
		return except(new MaybeNullException());
	}

	public static final <T> Maybe<T> nothing() {

		return new Maybe<T>() {

			@Override
			public boolean isNull() { return true; }

			@Override
			public T value() { throw new NullPointerException(); }

			@Override
			public T or(final T defaultValue) { return defaultValue; }

			@Override
			public Maybe<T> or(final Maybe<T> other) { return other; }

			@Override
			public String toString() {
				return "Nothing";
			}

			@Override
			public <E extends Exception> T except(final E ex) throws E {
				throw ex;
			}

			@Override
			public Iterator<T> iterator() {
				return Collections.<T>emptyList().iterator();
			}
		};
	}

	public static final <T> Maybe<T> just(final T value) {
		if(value == null) throw new NullPointerException();

		return new Maybe<T>() {

			@Override
			public boolean isNull() { return false; }

			@Override
			public T value() { return value; }

			@Override
			public T or(final T defaultValue) { return value(); }

			@Override
			public Maybe<T> or(final Maybe<T> other) { return this; }

			@Override
			public String toString() {
				return "Just(" + value() + ")";
			}

			@Override
			public <E extends Exception> T except(final E ex) throws E {
				return value();
			}

			@Override
			public Iterator<T> iterator() {
				return Collections.singletonList(value()).iterator();
			}
		};
	}

	public static final <T> Maybe<T> from(final T value) {
		if(value == null)
			return nothing();
		else
			return just(value);
	}
}
