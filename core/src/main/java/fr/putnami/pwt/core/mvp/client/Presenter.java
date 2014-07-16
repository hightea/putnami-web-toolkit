package fr.putnami.pwt.core.mvp.client;

import com.google.gwt.place.shared.Place;
import com.google.gwt.user.client.ui.AcceptsOneWidget;


public interface Presenter {

	public <P extends Place> void present(P place, AcceptsOneWidget displayer);
}
