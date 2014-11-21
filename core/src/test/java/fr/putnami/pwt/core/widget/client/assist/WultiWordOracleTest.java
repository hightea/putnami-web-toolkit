package fr.putnami.pwt.core.widget.client.assist;

import com.google.common.collect.Lists;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.junit.Test;

import java.util.Collection;
import java.util.List;

import fr.putnami.pwt.core.widget.client.assist.Oracle.Callback;
import fr.putnami.pwt.core.widget.client.assist.Oracle.Request;
import fr.putnami.pwt.core.widget.client.assist.Oracle.Response;
import fr.putnami.pwt.core.widget.client.assist.Oracle.Suggestion;

public class WultiWordOracleTest {

	class SimpleCallback<T> implements Callback<T> {

		private Response<T> reponse;

		public int getMoreSuggestionsCount() {
			return reponse.getMoreSuggestionsCount();
		}

		public Collection<? extends Suggestion<T>> getSuggestions() {
			return reponse.getSuggestions();
		}

		public boolean hasMoreSuggestions() {
			return reponse.hasMoreSuggestions();
		}

		@Override
		public void onSuggestionsReady(Request request, Response<T> response) {
			this.reponse = response;
		}

		public List<T> asList() {
			List<T> list = Lists.newArrayList();
			for (Suggestion<T> suggestion : reponse.getSuggestions()) {
				list.add(suggestion.getValue());
			}
			return list;
		}

		@Override
		public String toString() {
			return asList().toString();
		}

	}

	@Test
	public void testString() {
		MultiWordOracle<String> oracle = new MultiWordOracle<String>();

		oracle.add("aaaaa");
		oracle.add("aaaab");
		oracle.add("aaabb");
		oracle.add("aabbb");
		oracle.add("abbbb");
		oracle.add("bbbbb");
		oracle.add("aaaaa bbbbb");
		oracle.add("aa");

		assertNotNull(oracle);

		SimpleCallback<String> callback = new SimpleCallback<String>();
		// exclude "abbbb" and "bbbbb"
		oracle.requestSuggestions(new Oracle.Request("aa"), callback);

		assertNotNull(callback.getSuggestions());
		assertEquals(6, callback.getSuggestions().size());
		assertEquals(0, callback.asList().indexOf("aa"));
		assertEquals(1, callback.asList().indexOf("aaaaa"));
		assertEquals(5, callback.asList().indexOf("aabbb"));
	}

}
