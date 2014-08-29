package fr.putnami.pwt.tutorial.client;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.user.client.ui.RootPanel;

import fr.putnami.pwt.core.widget.client.OutputStaticText;

public class TutorialApp implements EntryPoint {

	@Override
	public void onModuleLoad() {
		RootPanel.get().add(new OutputStaticText("Hello Putnami"));
	}
}
