import { type NextPage, type InferGetServerSidePropsType } from "next";
import axios from "axios";
// import Head from "next/head";
// import Link from "next/link";

/* -- eslint-disable @typescript-eslint/no-unsafe-assignment */
export async function getServerSideProps() {

  // TODO: reduce request size somehow
  // ideas:
  // 1. separate in 2 endpoints
  // 2. filter/handling data on backend  <===
  // 3. use pagination (kinda hard)

  // const r = await axios.get("http://127.0.0.1:3000/api/back/books");
  const r = await axios.get("http://127.0.0.1:8000/books");
  if (r.status != 200) {
    throw new Error(`Failed to fetch data: ${r.status}`);
  }

  const data = r.data
  return {
    props: {
      data,
      aNumber: 42,
    },
  };
}

type Props = InferGetServerSidePropsType<typeof getServerSideProps>;

const Home: NextPage<Props> = ({ data, aNumber }) => {
  return (
    <main>
      <h1> Our first page </h1>
      <button className="btn btn-primary"> Do it! </button>
      <p> {JSON.stringify(data)} </p>
      {/* <Plot> </Plot> */}
    </main>
  );
};

export default Home;
