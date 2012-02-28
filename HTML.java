package extendedJava;

import java.io.PrintStream;
import java.util.*;
import java.util.Map.Entry;
import com.dataClasses.Either;

public final class HTML {

	public final String tag;
	public final Map<String, Object> attributes;
	public final List<Either<Object, HTML>> children;

	public HTML(final String tag, final Map<String, Object> attributes, final List<Either<Object, HTML>> children) {
		this.tag = tag;
		this.attributes = attributes;
		this.children = children;
	}

	public HTML add(final Object child) {
		this.children.add(Either.<Object, HTML>bad(child));
		
		return this;
	}

	public HTML add(final HTML child) {
		this.children.add(Either.<Object, HTML>good(child));
		
		return this;
	}

	public HTML add(final Iterable<HTML> children) {
		for(final HTML child : children) {
			add(child);
		}
		
		return this;
	}

	public HTML add(final HTML[] children) {
		for(final HTML child : children) {
			add(child);
		}
		
		return this;
	}

	public static final HTML full(String tag, final Map<String, Object> attributes) {
		return new HTML(tag, attributes, new ArrayList<Either<Object, HTML>>());
	}

	public static final HTML full(String tag) {
		return new HTML(tag, new HashMap<String, Object>(), new ArrayList<Either<Object, HTML>>());
	}

	public static final HTML closed(String tag) {
		return new HTML(tag, new HashMap<String, Object>(), null);
	}

	public static final HTML closed(String tag, final Map<String, Object> attributes) {
		return new HTML(tag, attributes, null);
	}




	public String toString() {

		String result = "<" + tag;

		for(final Entry<String, Object> entry : attributes.entrySet()) {
			result += " " + entry.getKey() + "=\"" + entry.getValue() + "\"";
		}

		if(children == null) {
			result += "/>";
		} else {
			result += ">";
			for(final Either<Object, HTML> sub : children) {
				if(sub.isBad()) {
					result += sub.badValue().toString();
				} else {
					result += sub.goodValue().toString();
				}
			}
			result += "</" + tag + ">";
		}

		return result;
	}

	public void print(final PrintStream out) {

		out.print("<" + tag);

		for(final Entry<String, Object> entry : attributes.entrySet()) {
			out.print(" " + entry.getKey() + "=\"" + entry.getValue() + "\"");
		}

		if(children == null) {
			out.print("/>");
		} else {
			out.print(">");
			for(final Either<Object, HTML> sub : children) {
				if(sub.isBad()) {
					out.println(sub.badValue());
				} else {
					sub.goodValue().print(out);
				}
			}
			out.print("</" + tag + ">");
		}
	}
}