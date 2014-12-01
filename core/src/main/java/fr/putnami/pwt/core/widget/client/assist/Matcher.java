package fr.putnami.pwt.core.widget.client.assist;

public interface Matcher<T> {

	int match(T value, String query);

}
