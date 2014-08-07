package fr.putnami.pwt.core.widget.client.util;

import com.google.gwt.dom.client.Element;

public final class SelectionUtils {

	public native static void disableTextSelectInternal(Element e, boolean disable)
	/*-{
	    if (disable) {
	      e.ondrag = function () { return false; };
	      e.onselectstart = function () { return false; };
	    } else {
	      e.ondrag = null;
	      e.onselectstart = null;
	    }
	  }-*/;

	private SelectionUtils() {

	}
}
