package fr.putnami.pwt.core.widget.client.boot;

import fr.putnami.pwt.core.theme.client.IconFont;

public class FontAwesomeIcons extends IconFont {

	protected FontAwesomeIcons() {
		super("theme/default/style/fontello.css", "icon-");

		addAlias("add", "plus");
		addAlias("save", "floppy");
		addAlias("view", "search");

		addAlias("drag", "menu");
	}

	@Override
	protected String transformClassName(String iStr) {
		return iStr.toLowerCase().replaceAll("_", "-");
	}

}
