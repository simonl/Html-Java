package com.xml;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.FactoryConfigurationError;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

public final class XML {
	private XML() { }

	public enum Type { TEXT, TAG }

	public static abstract class Node {
		public final Type type;

		private Node(final Type type) {
			this.type = type;
		}

		public String text() { throw new UnsupportedOperationException(); }
		public String tag() { throw new UnsupportedOperationException(); }
		public Map<String, Object> attributes() { throw new UnsupportedOperationException(); }
		public Maybe<? extends List<Node>> children() { throw new UnsupportedOperationException(); }

		public final String toString() {
			return XML.show("", this);
		}

		public final Node add(final Node child) {
			XML.add(this, child);

			return this;
		}

		public final Node add(final String text) {
			XML.add(this, text);

			return this;
		}

		public final Node add(final Object value) {
			XML.add(this, value);

			return this;
		}

		public final Node add(final Iterable<Node> children) {
			XML.add(this, children);

			return this;
		}

		public final Node add(final Node[] children) {
			XML.add(this, children);

			return this;
		}
	}




	public static final Node text(final String text) {
		return new Node(Type.TEXT) {
			@Override public final String text() { return text; }
		};
	}

	public static final Node tag(final String tag, final Map<String, Object> attributes, final Maybe<? extends List<Node>> children) {
		return new Node(Type.TAG) {
			@Override public final String tag() { return tag; }
			@Override public final Map<String, Object> attributes() { return attributes; }
			@Override public final Maybe<? extends List<Node>> children() { return children; }
		};
	}

	public static final Node open(final String tag, final Map<String, Object> attributes) {
		return tag(tag, attributes, Maybe.just(new ArrayList<Node>()));
	}

	public static final Node open(final String tag) {
		return open(tag, new HashMap<String, Object>());
	}

	public static final Node closed(String tag, final Map<String, Object> attributes) {
		return tag(tag, attributes, Maybe.<List<Node>>nothing());
	}

	public static final Node closed(final String tag) {
		return closed(tag, new HashMap<String, Object>());
	}



	public static final void add(final Node node, final Node child) {
		node.children().value().add(child);
	}

	public static final void add(final Node node, final String text) {
		add(node, XML.text(text));
	}

	public static final void add(final Node node, final Object value) {
		add(node, value.toString());
	}

	public static final void add(final Node node, final Iterable<Node> children) {
		for(final Node child : children) {
			add(node, child);
		}
	}

	public static final void add(final Node node, final Node[] children) {
		for(final Node child : children) {
			add(node, child);
		}
	}



	public static final String show(final String indent, final Node node) {
		if(node.type == Type.TEXT) return indent + node.text();

		String result = indent + "<" + node.tag();

		for(final Entry<String, Object> entry : node.attributes().entrySet()) {
			result += " " + entry.getKey() + "=\"" + entry.getValue() + "\"";
		}

		if(node.children().isNull()) {
			result += "/>\n";

			return result;
		}

		final List<Node> children = node.children().value();

		result += ">";

		if(children.size() == 1) {
			final Node child = children.get(0);
			if(child.type == Type.TEXT) {
				final String text = child.text();

				result += text;

				result += "</" + node.tag() + ">\n";

				return result;
			}
		}

		result += "\n";
		for(final Node child : children)
			result += show("\t" + indent, child);

		result += indent + "</" + node.tag() + ">\n";

		return result;
	}

	public static final org.w3c.dom.Node createNode(final Document document, final Node node) {
		if(node.type == Type.TEXT)
			return document.createTextNode(node.text());

		final Element element = document.createElement(node.tag());

		final Map<String, Object> attributes = node.attributes();
		for(final String key : attributes.keySet()) {
			element.setAttribute(key, attributes.get(key).toString());
		}

		if(node.children().isNull()) return element;

		element.appendChild(document.createTextNode(" "));
		for(final Node child : node.children().value()) {
			element.appendChild(createNode(document, child));
		}

		return element;
	}

    public static final Document createDocument(final Node node) {
    	if(node.type == Type.TEXT)
    		throw new IllegalArgumentException("Document root must be an tag node!");

        try {
            final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            final DocumentBuilder builder = factory.newDocumentBuilder();
            final DOMImplementation impl = builder.getDOMImplementation();

            final Document document = impl.createDocument(null, "TEMPORARY", null);
            final Element root = document.getDocumentElement();

            final org.w3c.dom.Node translated = createNode(document, node);
            document.replaceChild(translated, root);

            return document;

        } catch (FactoryConfigurationError e) {
            System.out.println("Could not locate a JAXP DocumentBuilderFactory class");
        } catch (ParserConfigurationException e) {
            System.out.println("Could not locate a JAXP DocumentBuilder class");
        }

        throw new RuntimeException("Error generating XML document");
    }
}