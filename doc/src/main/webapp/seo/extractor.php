<?php
        
$place = urldecode($_GET['place']);
if (empty($place)) {
	$place='index';
}
$cacheFolder='data';
$fileName=$cacheFolder.'/'.$place.'.html';

if (!file_exists($fileName)) {
        http_response_code(404);
}
else {
         readFile($fileName);
}

?>
