package fr.putnami.pwt.doc.client.application;

import com.google.gwt.core.client.GWT;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.inject.client.annotation.Templated;
import fr.putnami.pwt.core.mvp.client.View;
import fr.putnami.pwt.core.mvp.client.ViewDecorator;
import fr.putnami.pwt.core.widget.client.Affix;
import fr.putnami.pwt.core.widget.client.Container;
import fr.putnami.pwt.core.widget.client.NavSpy;
import fr.putnami.pwt.core.widget.client.OneWidgetPanel;
import fr.putnami.pwt.doc.client.social.widget.SocialBar;

@Templated
public class SummaryDecorator extends ViewDecorator implements View {


	public static SummaryDecorator get() {
		return GWT.create(SummaryDecorator.class);
	}

	@UiField
	Affix tableOfContentAffix;

	@UiField
	Container pageContainer;
	@UiField
	NavSpy tableOfContent;
	@UiField
	OneWidgetPanel headerContainer;
	@UiField
	OneWidgetPanel contentContainer;
	@UiField
	SocialBar socialBar;

	@Override
	public void setWidget(IsWidget w) {
		if (w instanceof Page) {
			Page page = (Page) w;
			headerContainer.setWidget(page.header);
			contentContainer.setWidget(page.content);
		}
		tableOfContent.redraw();
	}

}
