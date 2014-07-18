package fr.putnami.pwt.core.mvp.client;

import com.google.gwt.place.shared.Place;
import com.google.gwt.place.shared.PlaceTokenizer;
import com.google.gwt.user.client.ui.IsWidget;

public interface ViewProxy<P extends Place> extends PlaceTokenizer<P>, ActivityFactory {

	interface Callback {

		void showView(IsWidget view);
	}

	void getView(Callback callback);

}
