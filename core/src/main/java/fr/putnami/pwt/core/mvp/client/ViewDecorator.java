package fr.putnami.pwt.core.mvp.client;

import com.google.gwt.place.shared.Place;
import com.google.gwt.user.client.ui.AcceptsOneWidget;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.inject.client.annotation.PresentHandler;

public abstract class ViewDecorator implements AcceptsOneWidget, IsWidget {

	private IsWidget decoratorWidget;
	private IsWidget view;

	protected void initWidget(IsWidget decoratorWidget) {
		this.decoratorWidget = decoratorWidget;
	}

	public void setView(IsWidget view) {
		this.view = view;
	}

	@Override
	public Widget asWidget() {
		if (decoratorWidget == null) {
			throw new IllegalStateException("ViewDecorator.initWidget() need to be called");
		}
		return decoratorWidget.asWidget();
	}

	@PresentHandler
	public void present(Place place) {
		if (view instanceof Presenter) {
			Presenter presenter = (Presenter) view;
			presenter.present(place, this);
		}
	}

}
